extern "C" void abort (void);

#include "vbase8-21.h"

void check_C0 (C0 &x, int i)
{
  if (x.i0 != i)
    abort ();
}

void check_C1 (C1 &x, int i)
{
  if (x.i1 != i)
    abort ();
}

void check_C2 (C2 &x, int i)
{
  if (x.i2 != i)
    abort ();
}

void check_C3 (C3 &x, int i)
{
  if (x.i3 != i)
    abort ();
}

void check_C4 (C4 &x, int i)
{
  if (x.i4 != i)
    abort ();
}

void check_C5 (C5 &x, int i)
{
  if (x.i5 != i)
    abort ();
}

void check_C6 (C6 &x, int i)
{
  if (x.i6 != i)
    abort ();
}

void check_C7 (C7 &x, int i)
{
  if (x.i7 != i)
    abort ();
}

void check_C8 (C8 &x, int i)
{
  if (x.i8 != i)
    abort ();
}

void check_C9 (C9 &x, int i)
{
  if (x.i9 != i)
    abort ();
}
