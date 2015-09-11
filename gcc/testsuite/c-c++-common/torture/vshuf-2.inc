/* Test fragment for vectors of 2 elements.  */

#ifndef UNSUPPORTED

struct S
{
  V in;
  VI mask;
  V out;
};

struct S tests[] = {
  { { A, B }, { 0, 1 }, { A, B } },
  { { A, B }, { -16U, -1U }, { A, B } },
  { { A, B }, { 1, 0 }, { B, A } },
  { { A, B }, { 0, 0 }, { A, A } },
  { { X, Y }, { 1, 1 }, { Y, Y } },
  { { X, Y }, { 1, 0 }, { Y, X } },
};

struct S2
{
  V in1, in2;
  VI mask;
  V out;
};

struct S2 tests2[] = {
  { { A, B }, { X, Y }, { 0, 1 }, { A, B } },
  { { A, B }, { X, Y }, { 2, 3 }, { X, Y } },
  { { A, B }, { X, Y }, { 0, 2 }, { A, X } },
  { { A, B }, { X, Y }, { 2, 1 }, { X, B } },
  { { A, B }, { X, Y }, { 3, 0 }, { Y, A } },
  { { A, B }, { X, Y }, { 0, 0 }, { A, A } },
  { { A, B }, { X, Y }, { 3, 3 }, { Y, Y } },
};

#endif
