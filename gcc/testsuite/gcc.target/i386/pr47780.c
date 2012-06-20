/* PR debug/47780 */
/* { dg-do compile } */
/* { dg-options "-O -fgcse -fgcse-las -fstack-protector-all -fno-tree-ccp -fno-tree-dominator-opts -fcompare-debug -Wno-psabi" } */

typedef int V2SF __attribute__ ((vector_size (128)));

V2SF
foo (int x, V2SF a)
{
  V2SF b = a + (V2SF) {};
  while (x--)
    a += b;
  return a;
}
