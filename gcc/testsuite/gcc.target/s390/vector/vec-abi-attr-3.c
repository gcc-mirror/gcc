/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler "gnu_attribute 8, 2" } } */

typedef double v4df __attribute__((vector_size(32)));
typedef struct { v4df a; } s;

s
add (v4df a, v4df b, v4df c, v4df d,
     v4df e, v4df f, v4df g, v4df h, v4df i)
{
  s t;
  t.a = a + b + c + d + e + f + g + h + i;
  return t;
}
