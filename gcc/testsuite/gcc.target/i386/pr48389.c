/* PR middle-end/48389 */
/* { dg-do compile } */
/* { dg-options "-O -mtune=pentiumpro -Wno-abi" } */
/* { dg-require-effective-target ia32 } */
typedef float V2SF __attribute__ ((vector_size (128)));
V2SF foo (int x, V2SF a)
{
  V2SF b = {};
  if (x & 42)
    b = a;
  a += b;
  return a;
}
