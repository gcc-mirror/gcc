/* Checks that we don't get error: section type conflict with ‘put_page’.  */

/* { dg-do compile } */
/* { dg-options "-mindirect-branch=thunk-extern -mfunction-return=thunk-extern -mindirect-branch-table -O2" } */

int a;
int b (void);
void c (int);

static void
put_page (void)
{
  if (b ())
    c (a);
}

__attribute__ ((__section__ (".init.text"), __cold__)) void
d (void)
{
  put_page ();
  put_page ();
}
