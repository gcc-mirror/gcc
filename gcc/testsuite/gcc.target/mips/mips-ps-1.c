/* { dg-do run } */
/* { dg-options "-mpaired-single" } */

/* Test v2sf calculations */
#include <stdlib.h>
#include <stdio.h>

typedef float v2sf __attribute__ ((vector_size (8)));

v2sf A = {100, 200};

/* Init from float */
v2sf init (float a, float b)
{
  return (v2sf) {a, b};
}

/* Move between registers */
v2sf move (v2sf a)
{
  return a;
}

/* Load from memory */
v2sf load ()
{
  return A;
}

/* Store to memory */ 
void store (v2sf a)
{
  A = a;
}

/* Add */ 
v2sf add (v2sf a, v2sf b)
{
  return a + b;
}

/* Subtract */ 
v2sf sub (v2sf a, v2sf b)
{
  return a - b;
}

/* Negate */
v2sf neg (v2sf a)
{
  return - a;
}

/* Multiply */ 
v2sf mul (v2sf a, v2sf b)
{
  return a * b;
}

/* Multiply and add */ 
v2sf madd (v2sf a, v2sf b, v2sf c)
{
  return a * b + c;
}

/* Multiply and subtract */ 
v2sf msub (v2sf a, v2sf b, v2sf c)
{
  return a * b - c;
}

/* Negate Multiply and add */ 
v2sf nmadd (v2sf a, v2sf b, v2sf c)
{
  return - (a * b + c);
}

/* Negate Multiply and subtract */ 
v2sf nmsub (v2sf a, v2sf b, v2sf c)
{
  return - (a * b - c);
}

/* Conditional Move */ 
v2sf cond_move1 (v2sf a, v2sf b, long i)
{
  if (i > 0)
    return a;
  else
    return b;
}

/* Conditional Move */ 
v2sf cond_move2 (v2sf a, v2sf b, int i)
{
  if (i > 0)
    return a;
  else
    return b;
}

/* Conditional Move */ 
v2sf cond_move3 (v2sf a, v2sf b, float i)
{
  if (i > 0.0)
    return a;
  else
    return b;
}

/* Conditional Move */ 
v2sf cond_move4 (v2sf a, v2sf b, double i)
{
  if (i > 0.0)
    return a;
  else
    return b;
}

NOMIPS16 int main()
{
  v2sf a, b, c, d, e, f;
  float f1, f2;

  f1 = 1.2;
  f2 = 3.4;
  a = init (f1, f2);
  b = (v2sf) {1.2, 3.4};
  if (!__builtin_mips_upper_c_eq_ps (a, b) ||
      !__builtin_mips_lower_c_eq_ps (a, b))
    abort ();

  a = (v2sf) {1.2, 2.3};
  b = (v2sf) {5.3, 6.1};
  b = move (a);

  if (!__builtin_mips_upper_c_eq_ps (a, b) ||
      !__builtin_mips_lower_c_eq_ps (a, b))
    abort ();

  a = (v2sf) {1.2, 2.3};
  b = (v2sf) {5.3, 6.1};
  c = add (a, b);
  d = (v2sf) {6.5, 8.4};
  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = sub (a, b);
  d = (v2sf) {-4, 6};
  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = mul (a, b);
  d = (v2sf) {5, 72};
  if (!__builtin_mips_upper_c_eq_ps (c, d) ||
      !__builtin_mips_lower_c_eq_ps (c, d))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = (v2sf) {5, 6};
  d = madd (a, b, c);
  e = (v2sf) {10, 78};
  if (!__builtin_mips_upper_c_eq_ps (d, e) ||
      !__builtin_mips_lower_c_eq_ps (d, e))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = (v2sf) {5, 6};
  d = msub (a, b, c);
  e = (v2sf) {0, 66};
  if (!__builtin_mips_upper_c_eq_ps (d, e) ||
      !__builtin_mips_lower_c_eq_ps (d, e))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = (v2sf) {5, 6};
  d = nmadd (a, b, c);
  e = (v2sf) {-10, -78};
  if (!__builtin_mips_upper_c_eq_ps (d, e) ||
      !__builtin_mips_lower_c_eq_ps (d, e))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = (v2sf) {5, 6};
  d = nmsub (a, b, c);
  e = (v2sf) {0, -66};
  if (!__builtin_mips_upper_c_eq_ps (d, e) ||
      !__builtin_mips_lower_c_eq_ps (d, e))
    abort ();

  a = (v2sf) {98, 12};
  b = neg (a);
  c = (v2sf) {-98, -12};
  if (!__builtin_mips_upper_c_eq_ps (b, c) ||
      !__builtin_mips_lower_c_eq_ps (b, c))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = cond_move1 (a, b, 1000);
  if (!__builtin_mips_upper_c_eq_ps (c, a) ||
      !__builtin_mips_lower_c_eq_ps (c, a))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = cond_move2 (a, b, -1000);
  if (!__builtin_mips_upper_c_eq_ps (c, b) ||
      !__builtin_mips_lower_c_eq_ps (c, b))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = cond_move3 (a, b, 9.0);
  if (!__builtin_mips_upper_c_eq_ps (c, a) ||
      !__builtin_mips_lower_c_eq_ps (c, a))
    abort ();

  a = (v2sf) {1, 12};
  b = (v2sf) {5, 6};
  c = cond_move4 (a, b, -10.0);
  if (!__builtin_mips_upper_c_eq_ps (c, b) ||
      !__builtin_mips_lower_c_eq_ps (c, b))
    abort ();

  a = (v2sf) {5, 12};
  b = (v2sf) {5, 6};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = __builtin_mips_movt_c_eq_ps (a, b, c, d);
  f = (v2sf) {8, 123};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  a = (v2sf) {5, 12};
  b = (v2sf) {5, 6};
  c = (v2sf) {33, 123};
  d = (v2sf) {8, 78};
  e = __builtin_mips_movf_c_eq_ps (a, b, c, d);
  f = (v2sf) {33, 78};
  if (!__builtin_mips_upper_c_eq_ps (e, f) ||
      !__builtin_mips_lower_c_eq_ps (e, f))
    abort ();

  a = load();
  b = (v2sf) {100, 200};
  if (!__builtin_mips_upper_c_eq_ps (a, b) ||
      !__builtin_mips_lower_c_eq_ps (a, b))
    abort ();

  a = (v2sf) {123, 321};
  store (a);
  b = load();
  if (!__builtin_mips_upper_c_eq_ps (a, b) ||
      !__builtin_mips_lower_c_eq_ps (a, b))
    abort ();

  printf ("Test Passes\n");
  exit (0);
}
