/* { dg-do compile } */
/* { dg-options "-O2" } */
int a;
/* { dg-message "note: previous declaration" "previous declaration" { target *-*-* } .-1 } */
void fn1 ()
{
  a++;
}
int a[] = {2};  /* { dg-error "conflicting types" } */
