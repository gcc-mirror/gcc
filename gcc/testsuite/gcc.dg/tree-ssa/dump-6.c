/* PR middle-end/90676 - default GIMPLE dumps lack information
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-store-merging" }
   { dg-require-effective-target int32plus }
   { dg-require-effective-target store_merge } */


extern __attribute__((aligned(2))) char a2[2];

void f2 (void)
{
  a2[0] = 1;
  a2[1] = 0;
}

extern __attribute__((aligned(4))) char a4[4];

void f4 (void)
{
  a4[0] = 1;
  a4[1] = 0;
  a4[2] = 0;
  a4[3] = 0;
}

extern __attribute__((aligned(8))) char a8[8];

void f8 (void)
{
  a8[0] = 1;
  for (int i = 1; i != 8; ++i)
    a8[i] = 0;
}

/* { dg-final { scan-tree-dump "MEM <unsigned short> \\\[\\(char \\*\\)\\&a2] = " "store-merging" } }
   { dg-final { scan-tree-dump "MEM <unsigned int> \\\[\\(char \\*\\)\\&a4] = " "store-merging" } }
   { dg-final { scan-tree-dump "MEM <unsigned int> \\\[\\(char \\*\\)\\&a8] = " "store-merging" { target { ilp32 } } } }
   { dg-final { scan-tree-dump "MEM <unsigned long> \\\[\\(char \\*\\)\\&a8] = " "store-merging" { target { lp64 } } } } */
