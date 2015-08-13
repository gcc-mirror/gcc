// { dg-do compile }

template <typename> class A;
template <typename _Tp> using __allocator_base = _Tp;
template <class T, class = T, class = int, class = __allocator_base<int>>
class B;
template <class T, class H, class P, class A>
bool operator==(B<T, H, P, A> const &, B<T, H, P, A> const &);
template <class T, class H, class P, class A>
bool operator!=(B<T, H, P, A> const &, B<T, H, P, A> const &);
typedef B<int> int_multiset;
int a;
template <typename> struct C {
    C(int) {}
};
template <typename> struct D;
template <typename> struct K;
struct L : C<A<D<int>>>, C<A<K<int>>> {
    template <typename First, typename Second>
	L(First, Second)
	: C<A<D<int>>>(0), C<A<K<int>>>(0) {}
};
template <typename Node> struct F {
    typedef typename Node::node_pointer node_pointer;
    node_pointer node_;
    F();
    F(typename Node::link_pointer p1) : node_(static_cast<node_pointer>(p1)) {}
    void operator++() { node_ = 0; }
    int operator!=(F p1) { return node_ != p1.node_; }
};
struct G {
    typedef G *link_pointer;
};
struct H {
    static int new_bucket_count(int) {
	int b;
	int *c = 0;
	if (a)
	  b = *c;
	return b;
    }
};
class functions {
public:
    functions(int, int) {}
    ~functions();
};
template <typename Types> struct table : functions {
    typedef typename Types::policy policy;
    typedef Types node_allocator;
    typedef F<typename Types::node> iterator;
    L allocators_;
    int bucket_count_;
    int size_;
    typename Types::link_pointer get_previous_start() const;
    iterator begin() const { return size_ ? get_previous_start() : 0; }
    table(int, typename Types::hasher, typename Types::key_equal, node_allocator)
	: functions(0, 0), allocators_(0, 0),
	bucket_count_(policy::new_bucket_count(0)), size_() {}
};
template <typename> struct K : G { typedef K *node_pointer; };
struct I {
    typedef G *link_pointer;
};
struct J {
    typedef I::link_pointer link_pointer;
};
template <typename> struct D {
    typedef int hasher;
    typedef int key_equal;
    typedef K<int> node;
    typedef J::link_pointer link_pointer;
    typedef H policy;
};
struct M : table<D<int>> {
    node_allocator grouped_table_impl_a;
    M(int, int) : table(0, 0, 0, grouped_table_impl_a) {}
    void equals(M const &) const {
	for (iterator d = begin(); d.node_;) {
	    iterator e;
	    group_equals(e);
	}
    }
    static int group_equals(iterator p1) {
	int f;
	iterator g;
	for (; g != p1; ++g)
	  if (find())
	    if (f)
	      return 0;
    }
    static int find();
};
template <class, class, class, class> class B {
    M table_;

public:
    B(unsigned long = 0);
    friend bool operator==<>(B const &, B const &);
    friend bool operator!=<>(B const &, B const &);
};
template <class T, class H, class P, class A>
B<T, H, P, A>::B(unsigned long)
     : table_(0, 0) {}
     template <class T, class H, class P, class A>
     bool operator==(B<T, H, P, A> const &p1, B<T, H, P, A> const &p2) {
	 p1.table_.equals(p2.table_);
     }
template <class T, class H, class P, class A>
bool operator!=(B<T, H, P, A> const &p1, B<T, H, P, A> const &p2) {
    p1.table_.equals(p2.table_);
}
void use_multiset_fwd_declared_function_typerun() {
    int_multiset x, y;
    x == y;
    x != y;
}
