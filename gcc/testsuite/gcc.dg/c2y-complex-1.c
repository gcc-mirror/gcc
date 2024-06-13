/* Test C2Y complex increment and decrement.  */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

_Complex float a, ax;
_Complex double b, bx;
_Complex long double c, cx;

int
main ()
{
  ax = a++;
  if (ax != 0
      || a != 1
      || __builtin_signbit (__builtin_crealf (ax))
      || __builtin_signbit (__builtin_cimagf (ax))
      || __builtin_signbit (__builtin_crealf (a))
      || __builtin_signbit (__builtin_cimagf (a)))
    abort ();
  a = __builtin_complex (0.0f, -0.0f);
  ax = a++;
  if (ax != 0
      || a != 1
      || __builtin_signbit (__builtin_crealf (ax))
      || !__builtin_signbit (__builtin_cimagf (ax))
      || __builtin_signbit (__builtin_crealf (a))
      || !__builtin_signbit (__builtin_cimagf (a)))
    abort ();
  a = 0;
  ax = ++a;
  if (ax != 1
      || a != 1
      || __builtin_signbit (__builtin_crealf (ax))
      || __builtin_signbit (__builtin_cimagf (ax))
      || __builtin_signbit (__builtin_crealf (a))
      || __builtin_signbit (__builtin_cimagf (a)))
    abort ();
  a = __builtin_complex (0.0f, -0.0f);
  ax = ++a;
  if (ax != 1
      || a != 1
      || __builtin_signbit (__builtin_crealf (ax))
      || !__builtin_signbit (__builtin_cimagf (ax))
      || __builtin_signbit (__builtin_crealf (a))
      || !__builtin_signbit (__builtin_cimagf (a)))
    abort ();
  a = 0;
  ax = a--;
  if (ax != 0
      || a != -1
      || __builtin_signbit (__builtin_crealf (ax))
      || __builtin_signbit (__builtin_cimagf (ax))
      || !__builtin_signbit (__builtin_crealf (a))
      || __builtin_signbit (__builtin_cimagf (a)))
    abort ();
  a = __builtin_complex (0.0f, -0.0f);
  ax = a--;
  if (ax != 0
      || a != -1
      || __builtin_signbit (__builtin_crealf (ax))
      || !__builtin_signbit (__builtin_cimagf (ax))
      || !__builtin_signbit (__builtin_crealf (a))
      || !__builtin_signbit (__builtin_cimagf (a)))
    abort ();
  a = 0;
  ax = --a;
  if (ax != -1
      || a != -1
      || !__builtin_signbit (__builtin_crealf (ax))
      || __builtin_signbit (__builtin_cimagf (ax))
      || !__builtin_signbit (__builtin_crealf (a))
      || __builtin_signbit (__builtin_cimagf (a)))
    abort ();
  a = __builtin_complex (0.0f, -0.0f);
  ax = --a;
  if (ax != -1
      || a != -1
      || !__builtin_signbit (__builtin_crealf (ax))
      || !__builtin_signbit (__builtin_cimagf (ax))
      || !__builtin_signbit (__builtin_crealf (a))
      || !__builtin_signbit (__builtin_cimagf (a)))
    abort ();

  bx = b++;
  if (bx != 0
      || b != 1
      || __builtin_signbit (__builtin_creal (bx))
      || __builtin_signbit (__builtin_cimag (bx))
      || __builtin_signbit (__builtin_creal (b))
      || __builtin_signbit (__builtin_cimag (b)))
    abort ();
  b = __builtin_complex (0.0, -0.0);
  bx = b++;
  if (bx != 0
      || b != 1
      || __builtin_signbit (__builtin_creal (bx))
      || !__builtin_signbit (__builtin_cimag (bx))
      || __builtin_signbit (__builtin_creal (b))
      || !__builtin_signbit (__builtin_cimag (b)))
    abort ();
  b = 0;
  bx = ++b;
  if (bx != 1
      || b != 1
      || __builtin_signbit (__builtin_creal (bx))
      || __builtin_signbit (__builtin_cimag (bx))
      || __builtin_signbit (__builtin_creal (b))
      || __builtin_signbit (__builtin_cimag (b)))
    abort ();
  b = __builtin_complex (0.0, -0.0);
  bx = ++b;
  if (bx != 1
      || b != 1
      || __builtin_signbit (__builtin_creal (bx))
      || !__builtin_signbit (__builtin_cimag (bx))
      || __builtin_signbit (__builtin_creal (b))
      || !__builtin_signbit (__builtin_cimag (b)))
    abort ();
  b = 0;
  bx = b--;
  if (bx != 0
      || b != -1
      || __builtin_signbit (__builtin_creal (bx))
      || __builtin_signbit (__builtin_cimag (bx))
      || !__builtin_signbit (__builtin_creal (b))
      || __builtin_signbit (__builtin_cimag (b)))
    abort ();
  b = __builtin_complex (0.0f, -0.0f);
  bx = b--;
  if (bx != 0
      || b != -1
      || __builtin_signbit (__builtin_creal (bx))
      || !__builtin_signbit (__builtin_cimag (bx))
      || !__builtin_signbit (__builtin_creal (b))
      || !__builtin_signbit (__builtin_cimag (b)))
    abort ();
  b = 0;
  bx = --b;
  if (bx != -1
      || b != -1
      || !__builtin_signbit (__builtin_creal (bx))
      || __builtin_signbit (__builtin_cimag (bx))
      || !__builtin_signbit (__builtin_creal (b))
      || __builtin_signbit (__builtin_cimag (b)))
    abort ();
  b = __builtin_complex (0.0f, -0.0f);
  bx = --b;
  if (bx != -1
      || b != -1
      || !__builtin_signbit (__builtin_creal (bx))
      || !__builtin_signbit (__builtin_cimag (bx))
      || !__builtin_signbit (__builtin_creal (b))
      || !__builtin_signbit (__builtin_cimag (b)))
    abort ();

  cx = c++;
  if (cx != 0
      || c != 1
      || __builtin_signbit (__builtin_creall (cx))
      || __builtin_signbit (__builtin_cimagl (cx))
      || __builtin_signbit (__builtin_creall (c))
      || __builtin_signbit (__builtin_cimagl (c)))
    abort ();
  c = __builtin_complex (0.0L, -0.0L);
  cx = c++;
  if (cx != 0
      || c != 1
      || __builtin_signbit (__builtin_creall (cx))
      || !__builtin_signbit (__builtin_cimagl (cx))
      || __builtin_signbit (__builtin_creall (c))
      || !__builtin_signbit (__builtin_cimagl (c)))
    abort ();
  c = 0;
  cx = ++c;
  if (cx != 1
      || c != 1
      || __builtin_signbit (__builtin_creall (cx))
      || __builtin_signbit (__builtin_cimagl (cx))
      || __builtin_signbit (__builtin_creall (c))
      || __builtin_signbit (__builtin_cimagl (c)))
    abort ();
  c = __builtin_complex (0.0L, -0.0L);
  cx = ++c;
  if (cx != 1
      || c != 1
      || __builtin_signbit (__builtin_creall (cx))
      || !__builtin_signbit (__builtin_cimagl (cx))
      || __builtin_signbit (__builtin_creall (c))
      || !__builtin_signbit (__builtin_cimagl (c)))
    abort ();
  c = 0;
  cx = c--;
  if (cx != 0
      || c != -1
      || __builtin_signbit (__builtin_creall (cx))
      || __builtin_signbit (__builtin_cimagl (cx))
      || !__builtin_signbit (__builtin_creall (c))
      || __builtin_signbit (__builtin_cimagl (c)))
    abort ();
  c = __builtin_complex (0.0L, -0.0L);
  cx = c--;
  if (cx != 0
      || c != -1
      || __builtin_signbit (__builtin_creall (cx))
      || !__builtin_signbit (__builtin_cimagl (cx))
      || !__builtin_signbit (__builtin_creall (c))
      || !__builtin_signbit (__builtin_cimagl (c)))
    abort ();
  c = 0;
  cx = --c;
  if (cx != -1
      || c != -1
      || !__builtin_signbit (__builtin_creall (cx))
      || __builtin_signbit (__builtin_cimagl (cx))
      || !__builtin_signbit (__builtin_creall (c))
      || __builtin_signbit (__builtin_cimagl (c)))
    abort ();
  c = __builtin_complex (0.0L, -0.0L);
  cx = --c;
  if (cx != -1
      || c != -1
      || !__builtin_signbit (__builtin_creall (cx))
      || !__builtin_signbit (__builtin_cimagl (cx))
      || !__builtin_signbit (__builtin_creall (c))
      || !__builtin_signbit (__builtin_cimagl (c)))
    abort ();

  exit (0);
}
