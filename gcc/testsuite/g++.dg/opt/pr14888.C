// PR target/14888
// This used to ICE because the truncdfsf2 isn't completely eliminated

// { dg-do compile }
// { dg-options "-O2 -ffast-math" }

class xcomplex
{
public:
  float re, im;

  xcomplex &operator*= (const float &fact)
  { re*=fact; im*=fact; return *this; }
};

void foo (xcomplex &almT, xcomplex &almG)
{
  double gb;
  almT*=gb;
  almG*=gb*42;
}

