/* PR c++/127559
   { dg-do compile { target c++11 } }
   { dg-additional-options -fdump-tree-original } */

/* Lambda objects that are subobjects of aggregates must be mapped, including
   their by-reference captures.  */

template <typename T> struct S { T x; int y; };
template <typename T> int baz (T &x) { return x.x (); }

void
f1 (int x)
{
  auto b = [&] { return x; };
  S <decltype (b)> c1 = { b, 42 };
  #pragma omp target
  if (c1.x () != 42)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump {map\(to:c1\.x \[len: [0-9]+\]\) map\(tofrom:\*c1\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:c1\.x\.__x \[pointer assign, bias: 0\]\)} original } } */

void
f2 (int x, int y)
{
  auto b = [&] { return x; };
  S <decltype (b)> d2[2] = { { b, 42 }, { b, 42 } };
  #pragma omp target
  if (d2[1].x () != 42 || d2[y].x () != 42)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump {map\(to:d2\[1\]\.x \[len: [0-9]+\]\) map\(tofrom:\*d2\[1\]\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:d2\[1\]\.x\.__x \[pointer assign, bias: 0\]\)} original } }
   { dg-final { scan-tree-dump {map\(to:d2\[y\]\.x \[len: [0-9]+\]\) map\(tofrom:\*d2\[y\]\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:d2\[y\]\.x\.__x \[pointer assign, bias: 0\]\)} original } } */

void
f3 (int x)
{
  auto b = [&] { return x; };
  S <S <decltype (b)> > e3 = { { b, 42 }, 42 };
  #pragma omp target
  if (e3.x.x () != 42)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump {map\(to:e3\.x\.x \[len: [0-9]+\]\) map\(tofrom:\*e3\.x\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:e3\.x\.x\.__x \[pointer assign, bias: 0\]\)} original } } */

void
f4 (int x)
{
  auto b = [&] { return x; };
  S <decltype (b)> c4 = { b, 42 };
  #pragma omp target
  if (baz (c4) != 42)
    __builtin_abort ();
}

/* No lambda-typed expression appears in the target region's body; see
   commit 84d5fecd2694ad70f391a20451d43d82fde37eb5
   "openmp c++: Temporary fix for some lambda mapping problems [PR127559]".
   { dg-final { scan-tree-dump {(?n)map\([^)]*c4} original { xfail *-*-* } } }
   Eventually might look as follows:
   { dg-final { scan-tree-dump {map\(to:c4\.x \[len: [0-9]+\]\) map\(tofrom:\*c4\.x\.__x \[len: [0-9]+\]\) map\(always_pointer:c4\.x\.__x \[pointer assign, bias: 0\]\)} original { xfail *-*-* } } } */
