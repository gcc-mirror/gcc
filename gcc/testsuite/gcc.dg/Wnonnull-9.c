/* { dg-do compile } */
/* { dg-options "-Wall" } */


void
foo (int a[static 1])
{
  if ((void*)0 == a)	/* { dg-warning "argument" "compared to NULL" } */
    return;
}

int
main ()
{
  foo ((void*)0);	/* { dg-warning "argument 1 null where non-null expected" } */
}

