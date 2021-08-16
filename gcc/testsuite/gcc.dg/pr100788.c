/* { dg-do compile } */
/* { dg-options "--coverage" } */

void
foo()
{
#line 1
}

int main()
{
  foo ();
}
