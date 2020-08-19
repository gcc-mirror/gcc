/* { dg-additional-options "-O1" } */

int l0;

class qv {
public:
  int operator[] (int b1) const { return k2[b1]; }

private:
  int *k2;
};

class g0 {
  qv nf, v6;

  void
  iq ();
};

void
g0::iq ()
{
  for (;;)
    if (nf[0] == 0)
      ++l0;
}
