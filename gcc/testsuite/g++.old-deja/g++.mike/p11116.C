// Build don't link:
// Special g++ Options: -Wno-pmf-conversions
// prms-id: 11116

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
// This test tests the rather strange property afforded one by G++ to
// peek inside a pointer-to-member, as if it were a structure.  We
// probably shouldn't allow that.  In any case, under the new ABI,
// the fields don't have the same names.
#else
class Bar {
public:
  int f(int a) { val = a; return val; }
private:
  int val;
};

typedef int (Bar::*BarPtr)(int);

void foo() {
  int a;
  int (Bar::*bp)(int) = &Bar::f;
  Bar bar;
  int (*p)(void *, int);

  p = (int (*)(void*,int))((void (*)())((bp).__pfn_or_delta2.__pfn));
  a = (*p)(&bar, 4);
}
#endif
