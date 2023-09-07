// PR c++/107198
// { dg-additional-options -fno-exceptions }

struct A {
  A() { throw 0; }		// { dg-error disabled }
  A(int i) { throw i; }
  A(const A&) { throw 10; }
};

void try_idx (int i)
{
  int t = 10;
  try {
    struct X {
      A e1[2], e2;
    }
    x2[3] = { { 1, 2, 3 }, { 4, 5, 6 } };
  } catch (int x) { t = x; }	// { dg-prune-output "not declared" }
}
