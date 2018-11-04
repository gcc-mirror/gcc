/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

char buf[40];
void test (int x)
{
  __builtin_strcpy (buf, "test");
  __builtin___strcat_chk (buf, "postfix" + x, sizeof (buf));
}

/* { dg-final { scan-tree-dump "memcpy_chk.*, 36\\)" "optimized" } } */
