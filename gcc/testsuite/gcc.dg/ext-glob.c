/* Test for the warning about external functions with non-global
   types.  In -traditional mode, these functions are globally visible
   even if declared in an inner scope, so their return types should
   also be visible.  */

/* { dg-do compile } */
/* { dg-options -traditional } */
/* { dg-warning "-traditional is deprecated" "deprecation warning" { target *-*-* } 0 } */

int
main ()
{
  struct foo { int a, b; };

  extern struct foo *bar();  /* { dg-warning "type of external" "good warn" } */
  extern int baz();	     /* { dg-bogus   "type of external" "bad warn"  } */

  return 0;
}
