/* Verify proper errors are generated for conflicted interrupt type.  */
/* { dg-do compile } */
/* { dg-options "" } */
void __attribute__ ((interrupt ("supervisor")))
foo(void);

void __attribute__ ((interrupt ("machine")))
foo (void)
{ /* { dg-error "function cannot have different interrupt type" } */
}
