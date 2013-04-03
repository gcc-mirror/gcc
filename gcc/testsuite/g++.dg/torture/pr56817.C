// { dg-do compile }
// { dg-options "--param max-unroll-times=32" }

struct A {};
A **q;
struct B
{
  A **j;
  B () { j = q; }
  A *& operator[] (unsigned long x) { return j[x]; }
};
struct C
{
  C (int r) : v (), s (r) {}
  A *& operator () (int i, int j) { return v[i * s + j]; }
  B v;
  int s;
};
struct D
{
  D ()
    {
      unsigned h = 2;
      for (int i = 0; i < 1; ++i, h *= 2)
	{
	  C w (h);
	  for (unsigned j = 0; j < h; ++j)
	    for (unsigned k = 0; k < h; ++k)
	      w (j, k) = new A;
	}
    }
};
void
foo ()
{
  for (int i = 0; i < 3; i++)
    A (), A (), D ();
}
