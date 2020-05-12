/* { dg-do compile } */
/* { dg-options "-mavx512vl" } */

int __attribute__((__vector_size__(16))) v;

void
foo(void)
{
  0 <= (0 != v) >= 0;
}
