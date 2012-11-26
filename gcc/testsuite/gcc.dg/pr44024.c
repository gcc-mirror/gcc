/* { dg-do link } */
/* { dg-options "-fdelete-null-pointer-checks -fdump-tree-original" } */

void foo();

int main()
{
  if (foo == (void *)0)
    link_error ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "foo" "original" { target { ! avr*-*-* } } } } */
/* { dg-final { cleanup-tree-dump "original" } } */
