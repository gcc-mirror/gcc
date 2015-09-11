// { dg-do compile }
// { dg-options "-O2 -fdump-tree-cddce1" }

struct Bar
{
  int i;
  ~Bar() { }
};
void bar_dtor_loop(Bar* p, unsigned int n)
{
  if (p) {
      Bar* e = p + n;
      while (e > p) {
	  --e;
	  e->~Bar();
      }
  }
}

// The clobber in ~Bar should persist but those inlined into
// bar_dtor_loop not, nor should the loop therein

// { dg-final { scan-tree-dump-times "CLOBBER" 1 "cddce1" } }
// { dg-final { scan-tree-dump-times "if" 0 "cddce1" } }
