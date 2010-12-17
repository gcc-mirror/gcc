/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-esra-details" } */

struct S
{
  int i;
  int j;
  char c[32]; /* this disables total scalarization */
};

extern struct S bar(void);

int foo1 (int b)
{
   struct S s1;

   s1 = bar ();
   return s1.i;
}

extern struct S *g;

int foo2 (void)
{
   struct S s2;

   s2 = *g;
   return s2.i;
}

/* { dg-final { scan-tree-dump-times "Created a replacement for s1" 0 "esra"} } */
/* { dg-final { scan-tree-dump-times "Created a replacement for s2" 1 "esra"} } */
/* { dg-final { cleanup-tree-dump "esra" } } */
