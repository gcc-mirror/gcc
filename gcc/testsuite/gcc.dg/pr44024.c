/* { dg-do link } */
/* { dg-options "-O1 -fdelete-null-pointer-checks -fdump-tree-ccp1" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

void foo();
void link_error (void);

int main()
{
  if (foo == (void *)0)
    link_error ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "if \\(foo" "ccp1" { target { ! avr*-*-* } } } } */
