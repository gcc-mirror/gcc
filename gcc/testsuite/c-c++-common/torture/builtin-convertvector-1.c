/* { dg-do run } */
/* { dg-skip-if "double support is incomplete" { "avr-*-*" } } */

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);
typedef int v4si __attribute__((vector_size (4 * sizeof (int))));
typedef unsigned int v4usi __attribute__((vector_size (4 * sizeof (unsigned int))));
typedef float v4sf __attribute__((vector_size (4 * sizeof (float))));
typedef double v4df __attribute__((vector_size (4 * sizeof (double))));
typedef long long v256di __attribute__((vector_size (256 * sizeof (long long))));
typedef double v256df __attribute__((vector_size (256 * sizeof (double))));

void
f1 (v4usi *x, v4si *y)
{
  *y = __builtin_convertvector (*x, v4si);
}

void
f2 (v4sf *x, v4si *y)
{
  *y = __builtin_convertvector (*x, v4si);
}

void
f3 (v4si *x, v4sf *y)
{
  *y = __builtin_convertvector (*x, v4sf);
}

void
f4 (v4df *x, v4si *y)
{
  *y = __builtin_convertvector (*x, v4si);
}

void
f5 (v4si *x, v4df *y)
{
  *y = __builtin_convertvector (*x, v4df);
}

void
f6 (v256df *x, v256di *y)
{
  *y = __builtin_convertvector (*x, v256di);
}

void
f7 (v256di *x, v256df *y)
{
  *y = __builtin_convertvector (*x, v256df);
}

void
f8 (v4df *x)
{
  v4si a = { 1, 2, -3, -4 };
  *x = __builtin_convertvector (a, v4df);
}

int
main ()
{
  union U1 { v4si v; int a[4]; } u1;
  union U2 { v4usi v; unsigned int a[4]; } u2;
  union U3 { v4sf v; float a[4]; } u3;
  union U4 { v4df v; double a[4]; } u4;
  union U5 { v256di v; long long a[256]; } u5;
  union U6 { v256df v; double a[256]; } u6;
  int i;
  for (i = 0; i < 4; i++)
    u2.a[i] = i * 2;
  f1 (&u2.v, &u1.v);
  for (i = 0; i < 4; i++)
    if (u1.a[i] != i * 2)
      abort ();
    else
      u3.a[i] = i - 2.25f;
  f2 (&u3.v, &u1.v);
  for (i = 0; i < 4; i++)
    if (u1.a[i] != (i == 3 ? 0 : i - 2))
      abort ();
    else
      u3.a[i] = i + 0.75f;
  f2 (&u3.v, &u1.v);
  for (i = 0; i < 4; i++)
    if (u1.a[i] != i)
      abort ();
    else
      u1.a[i] = 7 * i - 5;
  f3 (&u1.v, &u3.v);
  for (i = 0; i < 4; i++)
    if (u3.a[i] != 7 * i - 5)
      abort ();
    else
      u4.a[i] = i - 2.25;
  f4 (&u4.v, &u1.v);
  for (i = 0; i < 4; i++)
    if (u1.a[i] != (i == 3 ? 0 : i - 2))
      abort ();
    else
      u4.a[i] = i + 0.75;
  f4 (&u4.v, &u1.v);
  for (i = 0; i < 4; i++)
    if (u1.a[i] != i)
      abort ();
    else
      u1.a[i] = 7 * i - 5;
  f5 (&u1.v, &u4.v);
  for (i = 0; i < 4; i++)
    if (u4.a[i] != 7 * i - 5)
      abort ();
  for (i = 0; i < 256; i++)
    u6.a[i] = i - 128.25;
  f6 (&u6.v, &u5.v);
  for (i = 0; i < 256; i++)
    if (u5.a[i] != i - 128 - (i > 128))
      abort ();
    else
      u5.a[i] = i - 128;
  f7 (&u5.v, &u6.v);
  for (i = 0; i < 256; i++)
    if (u6.a[i] != i - 128)
      abort ();
  f8 (&u4.v);
  for (i = 0; i < 4; i++)
    if (u4.a[i] != (i >= 2 ? -1 - i : i + 1))
      abort ();
  return 0;
}
