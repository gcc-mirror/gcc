// { dg-do run }
template <typename _Tp, long _Nm> struct A {
    typedef _Tp _Type[_Nm];
    static _Tp &_S_ref(const _Type &p1, int p2) {
	return const_cast<_Tp &>(p1[p2]);
    }
};
template <typename _Tp, long _Nm> struct B {
    typedef A<_Tp, _Nm> _AT_Type;
    typename _AT_Type::_Type _M_elems;
    _Tp &operator[](long p1) const { return _AT_Type::_S_ref(_M_elems, p1); }
};
int t;
void foo(int p1, int &p2) {
    if ((t & 1) == 0) {
	if (p1 != 1)
	  __builtin_abort();
	if (p2 != 2)
	  __builtin_abort();
    }
    t++;
}
__attribute__((noinline))
     void test1(const B<int, 2> &p1) { foo(p1[0], p1[1]); }
     void test(B<B<int, 2>, 2> &p1) {
	 test1(p1[0]);
	 test1(p1[1]);
	 foo(p1[0][0], p1[0][1]);
     }
int main() {
    B<B<int, 2>, 2> t;
    t[0][0] = 1;
    t[0][1] = 2;
    test(t);
}
