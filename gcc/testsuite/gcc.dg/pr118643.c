/* { dg-do compile } */
/* { dg-options "-O" } */

typedef __attribute__((__vector_size__(1))) unsigned char V;

V x;
void foo()
{
  V v = x;
  x = *(V *)(&v - 1);
}
