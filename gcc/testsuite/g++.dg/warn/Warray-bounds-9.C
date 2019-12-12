/* PR c++/88565 - enhance -Warray-bounds for C++ trailing class member arrays
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct S0
{
  S0 ();

  int f ();

  int a[0];
};

int S0::f ()
{
  // The following is not diagnosed but should be in a class with a ctor.
  return a[0];      // { dg-warning "\\\[-Warray-bounds" "pr88565" { xfail *-*-* } }
}


struct S1
{
  S1 ();

  int f ();

  int a[1];
};

int S1::f ()
{
  // The following is only diagnosed with -Warray-bounds=2 but should
  // be even at level 1 in a call with a ctor.
  return a[1];      // { dg-warning "\\\[-Warray-bounds" "pr88565" { xfail *-*-* } }
}


struct S2
{
  S2 ();

  int f ();

  int a[2];
};

int S2::f ()
{
  return a[2];      // { dg-warning "\\\[-Warray-bounds" }
}


struct S3
{
  S3 ();

  int f ();

  int a[3];
};

int S3::f ()
{
  return a[3];      // { dg-warning "\\\[-Warray-bounds" }
}


struct Sx
{
  Sx ();

  int f ();

  int n, a[];
};

int Sx::f ()
{
  // The following is not diagnosed but should be in a class with a ctor.
  return a[0];      // { dg-warning "\\\[-Warray-bounds" "pr88565" { xfail *-*-* } }
}
