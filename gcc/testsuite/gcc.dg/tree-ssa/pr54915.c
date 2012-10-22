/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef double v2df __attribute__ ((__vector_size__ (16)));
typedef double v4df __attribute__ ((__vector_size__ (32)));

void f (v2df *ret, v4df* xp)
{
  v4df x = *xp;
  v2df xx = { x[2], x[3] };
  *ret = xx;
}
