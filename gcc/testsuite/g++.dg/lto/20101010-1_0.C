// { dg-lto-do link }
// { dg-lto-options "-Wno-return-type" }

typedef long size_t;
template < class, class > struct pair
{
}
;
template < typename > class allocator;
template < typename > struct equal_to;

template < class > struct hash;
template
<
class, class, class, class, class, class > struct dense_hashtable_iterator;
template
<
class,
    class, class, class, class, class > struct dense_hashtable_const_iterator;
template
<
class
Value,
    class
    Key,
    class
    HashFcn,
    class ExtractKey, class EqualKey, class Alloc > class dense_hashtable
{
public:
    typedef Key key_type;
    typedef Value value_type;
    typedef size_t size_type;
    typedef
	dense_hashtable_iterator
	< Value, Key, HashFcn, ExtractKey, EqualKey, Alloc > iterator;
    typedef
	dense_hashtable_const_iterator
	< Value, Key, HashFcn, ExtractKey, EqualKey, Alloc > const_iterator;
    static const size_type ILLEGAL_BUCKET = (-1);
    pair < size_type, size_type > find_position (key_type)
      {
	size_type insert_pos = ILLEGAL_BUCKET;
      }
    pair < iterator, bool > insert_noresize (value_type obj)
      {
	pair < size_type, size_type > pos = find_position ((obj));
      }
    pair < iterator, bool > insert (value_type & obj)
      {
	insert_noresize (obj);
      }
    ExtractKey get_key;
}

;
template
<
class
Value,
    class
    HashFcn
    =
    hash
    <
    Value
    >,
    class
    EqualKey
    =
    equal_to < Value >, class Alloc = allocator < Value > >class dense_hash_set
{
  struct Identity
    {
    }
  ;
  typedef
      dense_hashtable < Value, Value, HashFcn, Identity, EqualKey, Alloc > ht;
  ht rep;
public:
  typedef typename ht::value_type value_type;
  typedef typename ht::const_iterator iterator;
  pair < iterator, bool > insert (value_type obj)
    {
      pair < typename ht::iterator, bool > p = rep.insert (obj);
    }
}

;
class blah_46
{
}
;
struct foo_10:dense_hash_set < blah_46 >
{
}
;
class foo_14
{
  void hmmmmh_5 (blah_46);
  foo_10 negative_rrrrrrr_type_data_;
}
;
void
foo_14::hmmmmh_5 (blah_46 hahaha_id)
{
  negative_rrrrrrr_type_data_.insert (hahaha_id);
}

int main () { return 0; }
