/* Invalid __thread specifiers.  As diag-4.c but some cases in
   different orders.  */

__thread typedef int g4;	/* { dg-error "'__thread' used with 'typedef'" } */

void foo()
{
  __thread auto int l2;		/* { dg-error "'__thread' used with 'auto'" } */
  __thread register int l4;	/* { dg-error "'__thread' used with 'register'" } */
}
