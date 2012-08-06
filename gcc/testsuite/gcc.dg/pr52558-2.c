/* { dg-do compile } */
/* { dg-options "--param allow-store-data-races=0 -O2 -fdump-tree-lim1" } */

/* Test that g_2 is not written to unless !g_1.  */

int g_1 = 1;
int g_2 = 0;

int func_1(void)
{
 int l;
 for (l = 0; l < 1234; l++)
 {
   if (g_1)
     return l;
   else
     g_2 = 0;
 }
 return 999;
}

/* { dg-final { scan-tree-dump-times "MEM.*g_2_lsm_flag" 1 "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
