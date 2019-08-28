/* Verify proper errors are generated for invalid code.  */
void __attribute__ ((interrupt, naked))
foo (void)
{ /* { dg-error "are mutually exclusive" } */
}
