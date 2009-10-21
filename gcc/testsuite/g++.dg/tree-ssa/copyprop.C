// PR 39548 verify ssa ICE
//
// { dg-do compile { target { lp64 } } }
// { dg-options  "-Wno-error -fno-exceptions -fno-tree-vrp -O2 -fprofile-generate  -finline-limit=500"  } 
//

#include <map>
#include <vector>
#include <iostream>
#include <cstdlib>
using namespace std;
template<typename _FIter, typename _Tp> _FIter lower_bound(_FIter, _FIter, _Tp&);
template<class _Key> struct hash { };
template<class _Val> struct _Hashtable_node {
  _Hashtable_node* _M_next;
  _Val _M_val;
};
static const unsigned long __stl_prime_list[] = { 2, 3, 5 };
inline unsigned long prime(unsigned long __n)   {
  const unsigned long* __first = __stl_prime_list;
  const unsigned long* __last = __stl_prime_list + 29;
  const unsigned long* pos = lower_bound(__first, __last, __n);
  return pos == __last ? *(__last - 1) : *pos;
}
template<class _Val, class _Key, class _HashFcn,     class _ExtractKey, class _EqualKey, class _Alloc>     struct hashtable  {
  typedef _Key key_type;
  typedef _Val value_type;
  typedef _HashFcn hasher;
  typedef _EqualKey key_equal;
  typedef size_t size_type;
  typedef value_type& reference;
  typedef _Hashtable_node<_Val> _Node;
  typedef typename _Alloc::template rebind<value_type>::other allocator_type;
  allocator_type get_allocator() const { }
  typedef typename _Alloc::template rebind<_Node>::other _Node_Alloc;
  typedef typename _Alloc::template rebind<_Node*>::other _Nodeptr_Alloc;
  typedef vector<_Node*, _Nodeptr_Alloc> _Vector_type;
  _Node_Alloc _M_node_allocator;
  void _M_put_node(_Node* __p) {
    _M_node_allocator.deallocate(__p, 1);
  }
  hasher _M_hash;
  key_equal _M_equals;
  _ExtractKey _M_get_key;
  _Vector_type _M_buckets;
  size_type _M_num_elements;
  hashtable(size_type __n, const _HashFcn& __hf,   const _EqualKey& __eql,   const allocator_type& __a = allocator_type())  : _M_num_elements(0)  {
    _M_initialize_buckets(__n);
  }
  ~hashtable() { clear(); }
  reference  find_or_insert(const value_type& __obj);
  size_type  count(const key_type& __key) const {
    const size_type __n = _M_bkt_num_key(__key);
    size_type __result = 0;
    for (const _Node* __cur = _M_buckets[__n]; __cur; __cur = __cur->_M_next)
      if (_M_equals(_M_get_key(__cur->_M_val), __key))   ++__result;
  }
  size_type erase(const key_type& __key);
  void clear();
  size_type _M_next_size(size_type __n) const { return prime(__n); }
  void  _M_initialize_buckets(size_type __n)       {
    const size_type __n_buckets = _M_next_size(__n);
    _M_buckets.reserve(__n_buckets);
    _M_buckets.insert(_M_buckets.end(), __n_buckets, (_Node*) 0);
  }
  size_type       _M_bkt_num_key(const key_type& __key) const  {
    return _M_bkt_num_key(__key, _M_buckets.size());
  }
  size_type       _M_bkt_num_key(const key_type& __key, size_t __n) const  {
    return _M_hash(__key) % __n;
  }
  void       _M_delete_node(_Node* __n)  {
    this->get_allocator().destroy(&__n->_M_val);
    _M_put_node(__n);
  }
};
template<class _Val, class _Key, class _HF, class _Ex, class _Eq, class _All>     typename hashtable<_Val, _Key, _HF, _Ex, _Eq, _All>::size_type     hashtable<_Val, _Key, _HF, _Ex, _Eq, _All>::     erase(const key_type& __key)     {
  const size_type __n = _M_bkt_num_key(__key);
  _Node* __first = _M_buckets[__n];
  if (__first)     _Node* __cur = __first;
}
template<class _Val, class _Key, class _HF, class _Ex, class _Eq, class _All>     void     hashtable<_Val, _Key, _HF, _Ex, _Eq, _All>::     clear()     {
  for (size_type __i = 0; __i < _M_buckets.size(); ++__i)  {
    _Node* __cur = _M_buckets[__i];
    while (__cur != 0)  { _M_delete_node(__cur); }
  }
}
template<class _Key, class _Tp, class _HashFn = hash<_Key>,     class _EqualKey = equal_to<_Key>, class _Alloc = allocator<_Tp> >   struct hash_map     {
  typedef hashtable<pair<const _Key, _Tp>,_Key, _HashFn,    _Select1st<pair<const _Key, _Tp> >,    _EqualKey, _Alloc> _Ht;
  _Ht _M_ht;
  typedef typename _Ht::key_type key_type;
  typedef typename _Ht::value_type value_type;
  typedef typename _Ht::hasher hasher;
  typedef typename _Ht::key_equal key_equal;
  typedef typename _Ht::size_type size_type;
  typedef typename _Ht::allocator_type allocator_type;
  hash_map()       : _M_ht(100, hasher(), key_equal(), allocator_type()) { }
  _Tp&       operator[](const key_type& __key)   {
    return _M_ht.find_or_insert(value_type(__key, _Tp())).second;
  }
  size_type count(const key_type& __key) const { return _M_ht.count(__key); }
  size_type erase(const key_type& __key) {
    return _M_ht.erase(__key);
  }
};
extern size_t strlen (__const char *__s);
template <class C> struct scoped_ptr {
  explicit scoped_ptr(C* p = __null) : ptr_(p) { delete ptr_; }
  void reset(C* p = __null) {
    if (p != ptr_) { delete ptr_; }
  }
  C& operator*() const {}
  C* operator->() const {}
  bool operator==(C* p) const { return ptr_ == p; }
  bool operator!=(C* p) const { return ptr_ != p; }
  C* ptr_;
};
namespace std {
class strstreambuf  : public basic_streambuf<char, char_traits<char> >      {
};
class strstream  : public basic_iostream<char>    {
 public:        int pcount() const;
  char* str();
  strstreambuf _M_buf;
};
};
const int INFO = 0,  WARNING = 1,  ERROR = 2,  FATAL = 3,  NUM_SEVERITIES = 4;
struct foo_1 {
  foo_1(string* str) : str_(str)  { }
  operator bool() const {
    return (__builtin_expect(str_ != __null, 0));
  }
  string* str_;
};
template<class t1, class t2> string* Makefoo_1(const t1& v1, const t2& v2, const char* names) {
  strstream ss;
  ss << names << " (" << v1 << " vs. " << v2 << ")";
  return new string(ss.str(), ss.pcount());
}
template <class t1, class t2> inline string* Check_GTImpl(const t1& v1, const t2& v2, const char* names) {
  if (v1 > v2) return __null;
  else return Makefoo_1(v1, v2, names);
}
struct blah_54 {
  blah_54(const char* file, int line, int severity);
  ~blah_54();
  ostream& stream() { };
};
class blah_0  : public blah_54  {
 public:   blah_0(const char* file, int line);
  blah_0(const char* file, int line, const foo_1& result);
};
template <class Value, class Key, class HashFcn,  class ExtractKey, class EqualKey, class Alloc> class dense_hashtable;
template <class V, class K, class HF, class ExK, class EqK, class A> struct dense_hashtable_iterator {
  typedef V* pointer;
  dense_hashtable_iterator(const dense_hashtable<V,K,HF,ExK,EqK,A> *h, pointer it, pointer it_end, bool advance)        :    ht(h),    pos(it),    end(it_end)     {
    if (advance)        advance_past_empty_and_deleted();
  }
  pointer operator->() const { }
  void advance_past_empty_and_deleted() {
    while ( pos != end && (ht->test_empty(*this) || ht->test_deleted(*this)) )  ++pos;
  }
  const dense_hashtable<V,K,HF,ExK,EqK,A> *ht;
  pointer pos, end;
};
template <class V, class K, class HF, class ExK, class EqK, class A> struct dense_hashtable_const_iterator {
  typedef dense_hashtable_iterator<V,K,HF,ExK,EqK,A> iterator;
  typedef dense_hashtable_const_iterator<V,K,HF,ExK,EqK,A> const_iterator;
  typedef const V& reference;
  typedef const V* pointer;
  dense_hashtable_const_iterator(const dense_hashtable<V,K,HF,ExK,EqK,A> *h,  pointer it, pointer it_end, bool advance)          :  ht(h),  pos(it),  end(it_end)  	{
    if (advance)  advance_past_empty_and_deleted();
  }
  dense_hashtable_const_iterator(const iterator &it)  :  pos(it.pos), end(it.end)  {}
  reference operator*() const  { return *pos; }
  pointer operator->() const {}
  void advance_past_empty_and_deleted() {
    while ( pos != end && (ht->test_empty(*this) || ht->test_deleted(*this))) ++pos;
  }
  const_iterator& operator++() { }
  bool operator!=(const const_iterator& it) const { }
  const dense_hashtable<V,K,HF,ExK,EqK,A> *ht;
  pointer pos, end;
};
template <class Value, class Key, class HashFcn,  class ExtractKey, class EqualKey, class Alloc> class dense_hashtable {
 public:   typedef Key key_type;
  typedef Value value_type;
  typedef HashFcn hasher;
  typedef EqualKey key_equal;
  typedef size_t size_type;
  typedef dense_hashtable_iterator<Value, Key, HashFcn,   ExtractKey, EqualKey, Alloc>   iterator;
  typedef dense_hashtable_const_iterator<Value, Key, HashFcn,  ExtractKey, EqualKey, Alloc>   const_iterator;
  static const float HT_OCCUPANCY_FLT;
  static const float HT_EMPTY_FLT;
  static const size_t HT_MIN_BUCKETS = 32;
  iterator end() {
    return iterator(this, table + num_buckets, table + num_buckets, true);
  }
  const_iterator end() const {
    return const_iterator(this, table + num_buckets, table+num_buckets,true);
  }
  void set_value(value_type* dst, const value_type& src) {
    new(dst) value_type(src);
  }
  void destroy_buckets(size_type first, size_type last) {
    for (; first != last; ++first) table[first].~value_type();
  }
 private:   void squash_deleted() {
    if ( num_deleted ) {
      dense_hashtable tmp(*this);
      swap(tmp);
    }
 }
  public:   void set_deleted_key(const value_type &val) { squash_deleted(); }
  bool test_deleted(size_type bucknum) const {
    return (use_deleted && num_deleted > 0 && equals(get_key(delval), get_key(table[bucknum])));
  }
  bool test_deleted(const const_iterator &it) const {
    return (use_deleted && num_deleted > 0 && equals(get_key(delval), get_key(*it)));
  }
  bool set_deleted(const_iterator &it) {
    set_value(const_cast<value_type*>(&(*it)), delval);
  }
  bool test_empty(size_type bucknum) const {
    return equals(get_key(emptyval), get_key(table[bucknum]));
  }
  bool test_empty(const const_iterator &it) const {
    return equals(get_key(emptyval), get_key(*it));
  }
  void fill_range_with_empty(value_type* table_start, value_type* table_end) {
    uninitialized_fill(table_start, table_end, emptyval);
  }
  void set_empty(size_type buckstart, size_type buckend) {
    destroy_buckets(buckstart, buckend);
    fill_range_with_empty(table + buckstart, table + buckend);
  }
  size_type size() const {
    return num_elements - num_deleted;
  }
  size_type bucket_count() const { }
  static const size_type ILLEGAL_BUCKET = size_type(-1);
  size_type min_size(size_type num_elts, size_type min_buckets_wanted) {
    size_type sz = HT_MIN_BUCKETS;
    while ( sz < min_buckets_wanted || num_elts >= sz * enlarge_resize_percent )  sz *= 2;
  }
  void maybe_shrink() {
    if (shrink_threshold > 0 &&  (num_elements-num_deleted) < shrink_threshold &&  bucket_count() > HT_MIN_BUCKETS ) {
      size_type sz = bucket_count() / 2;
      sz /= 2;
      dense_hashtable tmp(*this, sz);
      swap(tmp);
    }
  }
  void resize_delta(size_type delta, size_type min_buckets_wanted = 0) {
    if ( consider_shrink )       maybe_shrink();
    const size_type needed_size = min_size(num_elements + delta,  min_buckets_wanted);
    if ( needed_size > bucket_count() ) {
      const size_type resize_to = min_size(num_elements - num_deleted + delta,  min_buckets_wanted);
      dense_hashtable tmp(*this, resize_to);
      swap(tmp);
    }
  }
  void copy_from(const dense_hashtable &ht, size_type min_buckets_wanted = 0) {
    clear();
    const size_type resize_to = min_size(ht.size(), min_buckets_wanted);
    num_elements++;
  }
  explicit dense_hashtable(size_type n = 0, const HashFcn& hf = HashFcn(),       const EqualKey& eql = EqualKey(),const ExtractKey& ext = ExtractKey()) : num_deleted(0), use_deleted(false), use_empty(false), delval(),  emptyval(),      enlarge_resize_percent(HT_OCCUPANCY_FLT),      shrink_resize_percent(HT_EMPTY_FLT), table(__null), num_buckets(min_size(0, n)),  num_elements(0)  {
    reset_thresholds();
   }
   dense_hashtable(const dense_hashtable& ht, size_type min_buckets_wanted = 0) :   num_deleted(0), use_deleted(ht.use_deleted),   use_empty(ht.use_empty), delval(ht.delval), emptyval(ht.emptyval), enlarge_resize_percent(ht.enlarge_resize_percent),          shrink_resize_percent(ht.shrink_resize_percent),    table(__null),         num_buckets(0),    num_elements(0)  {
     reset_thresholds();
     copy_from(ht, min_buckets_wanted);
     set_value(&emptyval, ht.emptyval);
     enlarge_resize_percent = ht.enlarge_resize_percent;
     copy_from(ht);
   }
  ~dense_hashtable() {
    if (table) {
      destroy_buckets(0, num_buckets);
      free(table);
    }
  }
  void swap(dense_hashtable& ht) {
    std::swap(equals, ht.equals);
    {
      value_type tmp;
      set_value(&delval, ht.delval);
      set_value(&ht.delval, tmp);
      set_value(&ht.emptyval, tmp);
    }
    std::swap(table, ht.table);
    std::swap(num_buckets, ht.num_buckets);
    reset_thresholds();
    ht.reset_thresholds();
  }
  void clear() {
    if (table)  destroy_buckets(0, num_buckets);
    num_buckets = min_size(0,0);
    set_empty(0, num_buckets);
  }
  pair<size_type, size_type> find_position(const key_type &key) const {
    const size_type bucket_count_minus_one = bucket_count() - 1;
    size_type bucknum = hash(key) & bucket_count_minus_one;
    size_type insert_pos = ILLEGAL_BUCKET;
    while ( 1 ) {
      if ( test_empty(bucknum) ) {
        if ( insert_pos == ILLEGAL_BUCKET )  return pair<size_type,size_type>(ILLEGAL_BUCKET, insert_pos);
      }
      else if ( test_deleted(bucknum) ) {
        if ( insert_pos == ILLEGAL_BUCKET )            insert_pos = bucknum;
      }
      else if ( equals(key, get_key(table[bucknum])) ) {
        return pair<size_type,size_type>(bucknum, ILLEGAL_BUCKET);
      }
    }
  }
  iterator find(const key_type& key) {
    if ( size() == 0 ) return end();
    pair<size_type, size_type> pos = find_position(key);
    if ( pos.first == ILLEGAL_BUCKET )              return end();
    return iterator(this, table + pos.first, table + num_buckets, false);
  }
  const_iterator find(const key_type& key) const {
    if ( size() == 0 )         return end();
    pair<size_type, size_type> pos = find_position(key);
    if ( pos.first == ILLEGAL_BUCKET )                return end();
    return const_iterator(this, table + pos.first, table+num_buckets, false);
  }
  size_type count(const key_type &key) const {
    pair<size_type, size_type> pos = find_position(key); }
  pair<iterator, bool> insert_noresize(const value_type& obj) {
    const pair<size_type,size_type> pos = find_position(get_key(obj));
    if ( pos.first != ILLEGAL_BUCKET) {
      return pair<iterator,bool>(iterator(this, table + pos.first, table + num_buckets, false), false);
    }
    else {
      if ( test_deleted(pos.second) ) { ++num_elements; }
      return pair<iterator,bool>(iterator(this, table + pos.second, table + num_buckets, false), true);
    }
  }
  pair<iterator, bool> insert(const value_type& obj) {
    resize_delta(1);
    return insert_noresize(obj);
  }
  size_type erase(const key_type& key) {
    const_iterator pos = find(key);
    if ( pos != end() ) {
      set_deleted(pos);
    }
  }
  hasher hash;
  key_equal equals;
  ExtractKey get_key;
  size_type num_deleted;
  bool use_deleted;
  bool use_empty;
  value_type delval;
  value_type emptyval;
  float enlarge_resize_percent;
  float shrink_resize_percent;
  size_type shrink_threshold;
  size_type enlarge_threshold;
  value_type *table;
  size_type num_buckets;
  size_type num_elements;
  bool consider_shrink;
  void reset_thresholds() {
    enlarge_threshold = static_cast<size_type>(num_buckets  * shrink_resize_percent);
  }
};
template<> struct hash<long> {
  size_t operator()(long x) const {
  }
};
template<> struct hash<unsigned long> {
  size_t operator()(unsigned long x) const {
  }
};
template <class Key, class T, class HashFcn = hash<Key>,  class EqualKey = equal_to<Key>, class Alloc = allocator<T> > class dense_hash_map {
  struct SelectKey {
    const Key& operator()(const pair<const Key, T>& p) const {
      return p.first;
    }
  };
  typedef dense_hashtable<pair<const Key, T>, Key, HashFcn, SelectKey, EqualKey, Alloc> ht;
  ht rep;
 public:    typedef typename ht::key_type key_type;
  typedef T data_type;
  typedef typename ht::value_type value_type;
  typedef typename ht::size_type size_type;
  typedef typename ht::iterator iterator;
  typedef typename ht::const_iterator const_iterator;
  iterator end() {
    return rep.end();
  }
  iterator find(const key_type& key) { return rep.find(key); }
  data_type& operator[](const key_type& key) {
    iterator it = find(key);
    return insert(value_type(key, data_type())).first->second;
  }
  pair<iterator, bool> insert(const value_type& obj) {
    return rep.insert(obj);
  }
  void set_deleted_key(const key_type& key) {
    rep.set_deleted_key(value_type(key, data_type()));
  }
  size_type erase(const key_type& key) { return rep.erase(key); }
};
template <class Value, class HashFcn = hash<Value>, class EqualKey = equal_to<Value>, class Alloc = allocator<Value> > class dense_hash_set {
  struct Identity {
    const Value& operator()(const Value& v) const { return v; }
  };
  typedef dense_hashtable<Value, Value, HashFcn, Identity, EqualKey, Alloc> ht;
  ht rep;
 public:    typedef typename ht::key_type key_type;
  typedef typename ht::value_type value_type;
  typedef typename ht::size_type size_type;
  typedef typename ht::const_iterator iterator;
  size_type count(const key_type& key) const {
    return rep.count(key);
  }
  pair<iterator, bool> insert(const value_type& obj) {
    pair<typename ht::iterator, bool> p = rep.insert(obj);
  }
  size_type erase(const key_type& key) {
    return rep.erase(key);
  }
};
class linked_ptr_internal {
 public:   bool depart() { if (next_ == this) return true; }
  mutable linked_ptr_internal const* next_;
};
template <typename T> class linked_ptr {
 public:     explicit linked_ptr(T* ptr = __null) {
 }
  ~linked_ptr() { depart(); }
  T& operator*() const { }
  T* value_;
  linked_ptr_internal link_;
  void depart() {
    if (link_.depart()) delete value_;
  }
};
class blah_3 {
  const char* ptr_;
  int length_;
 public:   blah_3(const char* str) : ptr_(str), length_((str == __null) ? 0 : static_cast<int>(strlen(str))) { }
};
class blah_5;
class Bitmap {
 public:   Bitmap(unsigned int size) : array_size_(RequiredArraySize(size)) { }
  static unsigned int RequiredArraySize(unsigned int num_bits) { return (num_bits + 31) >> 5; }
   unsigned int array_size_;
};
enum blah_31 { CREATIVE_FORMAT_TEXT_NARROW,  kNumblah_31s  };
enum blah_33 { BLACKLISTED  }; 
template <typename EnumT> class blah_55;
typedef blah_55<blah_31> blah_31Set;
enum blah_36 { APPROVAL_STATUS_APPROVED, APPROVAL_STATUS_UNKNOWN };
enum blah_37 { hahah_INVALID, hahah_KEYWORD };
template<typename EnumT> class blah_55 {
 public:    blah_55(int enum_size);
  bool Insert(EnumT x);
  const int enum_size_;
  Bitmap elements_;
};
template<typename EnumT> blah_55<EnumT>::blah_55(int enum_size) :enum_size_(enum_size), elements_(enum_size)   {
  while (foo_1 _result = Check_GTImpl(1, 0, "enum_size" " " ">" " " "0")) blah_0(".h", 1902, _result).stream();
};
enum blah_38 {
  ttttttt_9,    };
class blah_46 {
 public:   blah_46()       :     hahaha_id_(0),             type_(hahah_INVALID),             approval_status_(APPROVAL_STATUS_APPROVED)     {
 }
  blah_46(long cid)       :     hahaha_id_(cid),             type_(hahah_INVALID),             approval_status_(APPROVAL_STATUS_APPROVED)    {
  }
  long id() const {
    return (static_cast<long>(hahaha_id_) << 16) >> 16;
  }
  static const blah_46 kBlacklistedID;
  bool operator == (const blah_46& x) const { return id() == x.id(); }
  bool operator < (const blah_46& x) const { return id() < x.id(); }
  long hahaha_id_ : 48;
  blah_37 type_ : 8;
  blah_36 approval_status_ : 4;
};
template <> struct hash<blah_46> {
  size_t operator()(const blah_46 &x) const {
    return size_t(x.id());
  }
};
class blah_57 {
 public:   blah_57();
  void AddReason(blah_33 reason, const blah_3& debug_str, const blah_46& hahaha_id, bool );
  void set_collects_multiple_reasons(bool t) { }
 private:   struct foo_3 {
   string reject_desc;
 };
  foo_3 first_reason_;
};
template <class T> struct foo_5   : public unary_function<T*, long> {
  long operator()(const T* p) const {
    long id = reinterpret_cast<long>(p);
    if (id < 2)  return -id;
  }
};
template <class T> class DensePtrSet : public dense_hashtable<T*, long,   hash<long>, foo_5<T>, equal_to<long>, allocator<T*> > {
 public:    DensePtrSet() {
   this->set_deleted_key(reinterpret_cast<T*>(1));
 }
  const T* Find(long key) const {
    typename DensePtrSet<T>::const_iterator it = this->find(key);
    return it != this->end() ? *it : __null;
  }
};
struct foo_7 {
  foo_7(bool spell_correction, bool query_broadening, bool previous_query, bool near_aaaaa, bool same_length, float mult, float exp_score)    :     shengmo_0(spell_correction),    shengmo_1(query_broadening),    shengmo_2(previous_query),    shengmo_3(near_aaaaa),    shengmo_4(same_length),    multiplier(mult),    expansion_score(exp_score)    {
  }
  int CompareSameKeywordMatch(const foo_7& compare) const;
  bool shengmo_0, shengmo_1, shengmo_2, shengmo_3, shengmo_4;
  float multiplier, expansion_score;
};
enum blah_41 {
  ACP_ECPM_EARLY = 2 };
struct foo_8  { unsigned int packed_ctr1; };
struct foo_9  { foo_9() {}};
class blah_16;
class blah_17;
class foo_12 { public:   foo_12() {}
  unsigned long hahaha_id() const {}
  unsigned int qbb_score() const {}
 private:   static const vector<blah_46> hmmmmh_4;
  long hahaha_id_ : 40;
};
class foo_13 {
 public:    typedef dense_hash_map<long, int> BestMap;
  foo_13() { best_rrrrrrr_.set_deleted_key(-1); }
  void erase(long ad_group_id)  {
    best_rrrrrrr_.erase(ad_group_id);
  }
  typedef BestMap::iterator iterator;
  typedef BestMap::const_iterator const_iterator;
  const_iterator begin() const  { }
  iterator end() { return best_rrrrrrr_.end(); }
  iterator find(long ad_group_id) { return best_rrrrrrr_.find(ad_group_id); }
   const foo_12& GetMatch(const_iterator it) const {}
  void hmmmmh_27(long ad_group_id, const foo_12& addme);
 private:   BestMap best_rrrrrrr_;
  vector<foo_12> rrrrrrr_buffer_;
};
struct foo_10  : public dense_hash_set<blah_46> {};
class foo_9Set : public DensePtrSet<foo_9> {};
typedef map<blah_46, foo_7*> foo_6Data;
typedef hash_map<long, linked_ptr<blah_57> > RejectedAdGroupMap;
enum blah_43 {};
class foo_14 {
 public:   foo_14(const unsigned int, const blah_16*, const int*);
  bool GathersMultipleRejectionReasons() const;
  void hmmmmh_30(blah_46 hahaha_id, blah_38 type);
  const foo_7* Insertfoo_6(const blah_46 hahaha_id, bool shengmo_0, bool shengmo_1, bool shengmo_2, bool shengmo_3, bool shengmo_4_rewrite, float multiplier, float context_score);
  void hmmmmh_7(blah_46 hahaha_id, blah_38 type);
  foo_9* Insertfoo_9();
  bool hmmmmh_8(long ad_group_id, const foo_12 &entry);
  void hmmmmh_9(long ad_group_id);
  foo_13::iterator hmmmmh_0(long ad_group_id);
  bool hmmmmh_8(long ad_group_id, foo_13::iterator best, const foo_12& entry);
  void hmmmmh_5(const blah_46 hahaha_id);
  void hmmmmh_29(const blah_46 hahaha_id);
  bool hmmmmh_12(const blah_46 hahaha_id) const;
  bool hmmmmh_13(const blah_46 hahaha_id) const;
  const foo_9* Getfoo_9(const blah_46 hahaha_id) const;
  bool Gathersfoo_9() const {}
  const foo_10* rrrrrrr_type_data() const {}
  const foo_10* negative_rrrrrrr_type_data() const {}
  const foo_10* positive_rrrrrrr_type_data() const {}
  const foo_9Set* kw_info_set() const { }
   const foo_6Data* rewrite_data() const {}
  const vector<blah_17>& query_rectangles() const {}
   void hmmmmh_14();
   void AddQueryRectangle(const blah_17& query_rectangle);
   void hmmmmh_15(long ad_group_id, const blah_46 hahaha_id,  blah_33 reject_class, const char* reject_desc = __null);
   void hmmmmh_16(const vector<long>& rejected_sssr_ids);
   void Copy(const foo_14& cmi);
   void hmmmmh_10();
 private:   const blah_16* ad_request_;
  const int* cr_query_;
  blah_43 gather_flags_;
  vector<blah_17> query_rectangles_;
  foo_10 rrrrrrr_type_data_;
  foo_9Set kw_info_set_;
  foo_6Data rewrite_data_;
  scoped_ptr<RejectedAdGroupMap> rejected_sssr_map_;
  foo_13 ad_group_rrrrrrr_data_;
  vector<blah_46> geo_hahaha_;
  bool geo_hahaha_is_sorted_;
  foo_10 negative_rrrrrrr_type_data_, positive_rrrrrrr_type_data_;
  scoped_ptr<foo_10> extra_hahaha_set_;
  int dimension_id_;
  blah_31Set creative_formats_;
  scoped_ptr<dense_hash_set<unsigned long> > near_aaaaa_rrrrrrr_fps_;
  blah_41 comparison_policy_;
  blah_46 next_virtual_hahaha_id_;
  vector<void*>* sub_queries_;
  bool allow_only_whitelisted_customers_, automatic_hahaha_rrrrrrr_;
  scoped_ptr<blah_5> kw_arena_, expanded_rrrrrrr_arena_;
};
class blah_19 {
  void hmmmmh_3();
  enum blah_45 {};
};
void blah_19::hmmmmh_3() {}
class blah_16 {
 public:   int near_aaaaa_rrrrrrr_fps_size() const {}
  unsigned long near_aaaaa_rrrrrrr_fps(int i) const {}
};
class blah_21 {
 protected:   blah_21(char* first_block, const size_t block_size, bool align_to_page);
  void* GetMemoryFallback(const size_t size, const int align);
  void* GetMemory(const size_t size, const int align) {
    if ( size > 0 && size < remaining_ && align == 1 ) {
      last_alloc_ = freestart_;
    }
    return GetMemoryFallback(size, align);
  }
  char* freestart_;
  char* last_alloc_;
  size_t remaining_;
};
class blah_5 : blah_21 {
 public:   char* Alloc(const size_t size) {
   return reinterpret_cast<char*>(GetMemory(size, 1));
 }
};
class blah_25 {
 public:   virtual ~blah_25();
};
class blah_17 : blah_25 { };
void Fillfoo_8(const foo_12& x2, struct foo_8* out) {
  out->packed_ctr1 = x2.qbb_score();
}
const vector<blah_46> foo_12::hmmmmh_4;
foo_14::foo_14(const unsigned int gather_flags,   const blah_16* ad_request, const int* cr_query):    ad_request_(ad_request),     cr_query_(cr_query),        gather_flags_(static_cast<blah_43>(gather_flags)),      geo_hahaha_is_sorted_(false),         dimension_id_(0),       creative_formats_(kNumblah_31s),         comparison_policy_(ACP_ECPM_EARLY),      sub_queries_(new vector<void*>()),         allow_only_whitelisted_customers_(false),       automatic_hahaha_rrrrrrr_(false) {
  hmmmmh_10();
}
void foo_14::hmmmmh_5(const blah_46 hahaha_id) {
  negative_rrrrrrr_type_data_.insert(hahaha_id);
}
void foo_14::hmmmmh_7(blah_46 hahaha_id, blah_38 type) { }
foo_13::iterator foo_14::hmmmmh_0(     long ad_group_id) {
  return ad_group_rrrrrrr_data_.find(ad_group_id);
}
bool foo_14::hmmmmh_8(long ad_group_id, foo_13::iterator best, const foo_12& entry) {
  rejected_sssr_map_->erase(ad_group_id);
  ad_group_rrrrrrr_data_.hmmmmh_27(ad_group_id, entry);
}
bool foo_14::hmmmmh_8(long ad_group_id, const foo_12& entry) {
  foo_13::iterator best = hmmmmh_0(ad_group_id);
}
void foo_14::hmmmmh_9(long ad_group_id) {
  ad_group_rrrrrrr_data_.erase(ad_group_id);
}
void foo_14::hmmmmh_10() {
  if (near_aaaaa_rrrrrrr_fps_ != __null) {
    blah_54(".cc", 226, WARNING).stream() << "";
    for (int j = 0;
         j < ad_request_->near_aaaaa_rrrrrrr_fps_size(); j++) {
      near_aaaaa_rrrrrrr_fps_->insert(ad_request_->near_aaaaa_rrrrrrr_fps(j));
    }
  }
}
const foo_7* foo_14::Insertfoo_6(const blah_46 hahaha_id, bool shengmo_0, bool shengmo_1, bool shengmo_2,  bool shengmo_3, bool shengmo_4_rewrite, float multiplier, float context_score) {
  if (rrrrrrr_type_data_.count(hahaha_id) > 0)   return __null;
  foo_7* new_info =  new(expanded_rrrrrrr_arena_->Alloc(sizeof(foo_7))) foo_7(shengmo_0,shengmo_1, shengmo_2, shengmo_3, shengmo_4_rewrite, multiplier, context_score);
  pair<foo_6Data::iterator, bool> status = rewrite_data_.insert( make_pair(hahaha_id, new_info));
  foo_7* inserted = status.first->second;
  if (!status.second) {
    if (inserted->CompareSameKeywordMatch(*new_info) < 0)   *inserted = *new_info;
  }
}
foo_9* foo_14::Insertfoo_9() {
  foo_9* info = new(kw_arena_->Alloc(sizeof(foo_9))) foo_9;
  if (Gathersfoo_9())     kw_info_set_.insert(info);
  creative_formats_.Insert(CREATIVE_FORMAT_TEXT_NARROW);
}
bool foo_14::hmmmmh_12(const blah_46 hahaha_id) const {
  if (rrrrrrr_type_data_.count(hahaha_id)) return true;
}
bool foo_14::hmmmmh_13(const blah_46 hahaha_id) const {
  if (positive_rrrrrrr_type_data_.count(hahaha_id)) return true;
}
const foo_9* foo_14::Getfoo_9(const blah_46 hahaha_id) const {
  if (Gathersfoo_9())     return kw_info_set_.Find(hahaha_id.id());
  static int occurrences_383 = 0, occurrences_mod_n_383 = 0;
  if (++occurrences_mod_n_383 > 1000)      occurrences_mod_n_383 -= 1000;
}
void foo_14::hmmmmh_15(long ad_group_id, const blah_46 hahaha_id,  blah_33 reject_class,  const char* reject_desc) {
  if (rejected_sssr_map_ == __null) {
    blah_54("a.cc", 413, ERROR).stream() << "re NULL";
    rejected_sssr_map_.reset(new RejectedAdGroupMap);
  }
  if (rejected_sssr_map_->count(ad_group_id) == 0) {
    blah_57* ad_rejection = new blah_57();
    ad_rejection->set_collects_multiple_reasons( GathersMultipleRejectionReasons());
    (*rejected_sssr_map_)[ad_group_id] = linked_ptr<blah_57>(ad_rejection);
  }
  blah_57& ad_rejection = *(*rejected_sssr_map_)[ad_group_id];
  ad_rejection.AddReason(reject_class, reject_desc, hahaha_id, false);
}
void foo_14::hmmmmh_16(const vector<long>& rejected_sssr_ids) {
  for (vector<long>::const_iterator it = rejected_sssr_ids.begin();
       it != rejected_sssr_ids.end(); ++it) {
    ad_group_rrrrrrr_data_.erase(*it);
    for (foo_13::const_iterator it = ad_group_rrrrrrr_data_.begin();
         it != ad_group_rrrrrrr_data_.end(); ++it) {
      hmmmmh_15(it->first,  ad_group_rrrrrrr_data_.GetMatch(it).hahaha_id(), BLACKLISTED);
    }
  }
  hmmmmh_30(blah_46::kBlacklistedID, ttttttt_9);
}
void foo_14::Copy(const foo_14& cmi) {
  rrrrrrr_type_data_ = *cmi.rrrrrrr_type_data();
  negative_rrrrrrr_type_data_ = *cmi.negative_rrrrrrr_type_data();
  positive_rrrrrrr_type_data_ = *cmi.positive_rrrrrrr_type_data();
  if (cmi.Gathersfoo_9()) {
    kw_info_set_ = *cmi.kw_info_set();
    rewrite_data_ = *cmi.rewrite_data();
  }
  hmmmmh_14();
  for (int i = 0; i < cmi.query_rectangles().size();
       ++i)  AddQueryRectangle(cmi.query_rectangles()[i]);
}
void foo_13::hmmmmh_27(long ad_group_id, const foo_12& addme) {
  int& best_index = best_rrrrrrr_[ad_group_id];
  rrrrrrr_buffer_.push_back(addme);
}
void foo_14::hmmmmh_29(const blah_46 hahaha_id) {
  if (extra_hahaha_set_ != __null) extra_hahaha_set_->erase(hahaha_id);
}
