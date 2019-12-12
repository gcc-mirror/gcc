// PR c++/89767
// { dg-do compile { target c++14 } }

void bar (int);

void
foo ()
{
  int x = 0;
  int a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0;
  auto z = [x, y = [x] { bar (x); }, x] { y (); bar (x); };	// { dg-error "already captured 'x' in lambda expression" }
  auto w = [x, a, b, c, d, y = [x] { bar (x); }, e, f, g, h, x] { y (); bar (x + a + b + c + d + e + f + g + h); };	// { dg-error "already captured 'x' in lambda expression" }
  z ();
  w ();
}
