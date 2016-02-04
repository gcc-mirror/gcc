/* { dg-do compile } */
/* { dg-options "-O -ffixed-ebp -mno-accumulate-outgoing-args" } */

/* { dg-warning "fixed ebp register requires" "" { target *-*-* } 0 } */

void foo (void);

int
main (int argc, char *argv[])
{
  foo ();
  return argc - 1;
}
