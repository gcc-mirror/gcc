/* PR 94131 - ICE on printf with a VLA string and -fno-tree-ccp
   -fno-tree-forwprop
   { dg-do compile }
   { dg-options "-O1 -fno-tree-ccp -fno-tree-forwprop" }
   { dg-require-effective-target alloca } */

void rv1 (int n)
{
  char a[n];
  __INTPTR_TYPE__ i = (__INTPTR_TYPE__ )&a[0];
  i &= 3;

  __builtin_memset (a, '\0', sizeof a);
  __builtin_printf ("%s", i ? &a[0] : "");
}


void sink (void*);

void rv2 (int n)
{
  char a[n];
  __INTPTR_TYPE__ i = (__INTPTR_TYPE__)&a[0];
  i &= 3;

  __builtin_memset (a, '\0', sizeof a);
  __builtin_printf ("%s", i ? &a[0] : "");

  sink (a);
}
