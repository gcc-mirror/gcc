/* Wide string literals should not be allowed in asm.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

int foo asm (L"bar"); /* { dg-error "wide string literal in 'asm'" } */

asm (L"foo"); /* { dg-error "wide string literal in 'asm'" } */

void
f (void)
{
  int x = 1;
  asm (L"foo"); /* { dg-error "wide string literal in 'asm'" } */
  asm ("foo" :
       L"=g" (x)); /* { dg-error "wide string literal in 'asm'" } */
  asm ("foo" : [x]
       L"=g" (x)); /* { dg-error "wide string literal in 'asm'" } */
  asm ("foo" : [x] "=g" (x),
       L"=g" (x)); /* { dg-error "wide string literal in 'asm'" } */
  asm ("foo" : :
       L"g" (x)); /* { dg-error "wide string literal in 'asm'" } */
  asm ("foo" : : :
       L"memory"); /* { dg-error "wide string literal in 'asm'" } */
  asm ("foo" : : : "memory",
       L"memory"); /* { dg-error "wide string literal in 'asm'" } */
}

/* Extra errors from the substitution of "" for wide strings: */
/* { dg-error "output" "output" { target *-*-* } 16 } */
/* { dg-error "output" "output" { target *-*-* } 18 } */
/* { dg-error "output" "output" { target *-*-* } 20 } */
/* { dg-warning "match" "match" { target *-*-* } 21 } */
/* { dg-error "register" "register" { target *-*-* } 23 } */
/* { dg-error "register" "register" { target *-*-* } 25 } */
