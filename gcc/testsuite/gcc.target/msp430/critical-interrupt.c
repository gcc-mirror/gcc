/* { dg-do compile } */
/* { dg-final { scan-assembler-not "attributes.*critical" } } */

void __attribute__((interrupt,critical))
fn1 (void)
{ /* { dg-warning "critical attribute has no effect on interrupt functions" } */
}

void __attribute__((critical,interrupt))
fn2 (void)
{ /* { dg-warning "critical attribute has no effect on interrupt functions" } */
}
