/* { dg-do compile } */
/* { dg-options "--coverage -Wno-error=coverage-invalid-line-number" } */

void
foo() // { dg-warning "function starts on a higher line number than it ends" }
{
#line 1
}

int main()
{
  foo ();
}
