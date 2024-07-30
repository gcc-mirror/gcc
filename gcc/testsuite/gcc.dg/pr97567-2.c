/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp -fdisable-tree-ethread" } */

char a[2];

extern int x;

void foo(void);

signed char g (signed char min, signed char max)
{
   signed char i = x;
   return i < min || max < i ? min : i;
}

void gg (void)
{
   signed char t = g (0, 9);
   /* Ranger should be able to remove the call to foo ().  */
   if (t > 9 || t < 0)
     foo ();
}

/* { dg-final { scan-tree-dump-not "foo" "evrp" } }  */
