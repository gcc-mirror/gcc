/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-vrp" } */

void foo (bool* a, int* b)
{
  for (int i = 0; i < 1000; ++i)
    {
      a[i] = i % 2;
      b[i] = i % 3;
    }
}

