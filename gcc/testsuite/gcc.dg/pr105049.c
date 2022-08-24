/* { dg-do compile } */
/* { dg-options "-O -fno-tree-forwprop" } */

typedef short __attribute__((__vector_size__ (sizeof(short)))) V;
typedef short __attribute__((__vector_size__ (2*sizeof(short)))) U;
char c;

U
foo (void)
{
  return __builtin_shufflevector ((V){}, (V){}, 0, 0) & c;
}
