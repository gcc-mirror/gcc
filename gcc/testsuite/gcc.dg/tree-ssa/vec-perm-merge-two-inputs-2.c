/* Once the permutes building d fold to one that reads only a and b,
   the uninitialized d is no longer used and there is nothing to warn
   about.  Building only part of d still warns.  */
/* { dg-do compile { target { aarch64*-*-* i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -Wuninitialized" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

typedef unsigned int v4su __attribute__((vector_size(16)));

v4su
f1 (v4su a, v4su b)
{
  v4su d;
  d[0] = a[2]; /* { dg-bogus "uninitialized" } */
  d[1] = a[3];
  d[2] = b[2];
  d[3] = b[3];
  return d;
}

v4su
f2 (v4su a)
{
  v4su d;
  d[0] = a[2]; d[1] = a[3]; /* { dg-warning "used uninitialized" } */
  return d;
}
