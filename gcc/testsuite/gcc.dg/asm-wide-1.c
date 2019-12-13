/* Wide string literals should not be allowed in asm.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

int foo asm (L"bar"); /* { dg-error "14:a wide string is invalid in this context" } */

asm (L"foo"); /* { dg-error "6:a wide string is invalid in this context" } */

void
f (void)
{
  int x = 1;
  asm (L"foo"); /* { dg-error "8:a wide string is invalid in this context" } */
  asm ("foo" :
       L"=g" (x)); /* { dg-error "8:a wide string is invalid in this context" } */
  /* Extra errors from the substitution of "" for wide strings: */
  /* { dg-error "output" "output" { target *-*-* } .-2 } */
  asm ("foo" : [x]
       L"=g" (x)); /* { dg-error "8:a wide string is invalid in this context" } */
  /* { dg-error "output" "output" { target *-*-* } .-1 } */
  asm ("foo" : [x] "=g" (x),
       L"=g" (x)); /* { dg-error "8:a wide string is invalid in this context" } */
  /* { dg-error "output" "output" { target *-*-* } .-1 } */
  asm ("foo" : :
       L"g" (x)); /* { dg-error "8:a wide string is invalid in this context" } */
  asm ("foo" : : :
       L"memory"); /* { dg-error "8:a wide string is invalid in this context" } */
  asm ("foo" : : : "memory",
       L"memory"); /* { dg-error "8:a wide string is invalid in this context" } */
}
