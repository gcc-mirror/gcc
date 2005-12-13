/* { dg-do compile } */
/* { dg-options "-maltivec" { target powerpc*-*-* } } */

__attribute__ ((vector_size (2))) signed char v1, v2, v3;
void
one (void)
{
  v1 = v2 + v3;
}

__attribute__ ((vector_size (8))) signed char v4, v5, v6;
void
two (void)
{
  v4 = v5 + v6;
}
