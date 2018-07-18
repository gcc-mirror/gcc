/* Verify proper errors are generated for invalid code.  */
int __attribute__ ((interrupt))
sub0 (void)
{ /* { dg-error "function cannot return a value" } */
  return 10;
}

void __attribute__ ((interrupt))
sub1 (int i)
{ /* { dg-error "function cannot have arguments" } */
}

void __attribute__ ((interrupt, naked))
sub2 (void)
{ /* { dg-error "are mutually exclusive" } */
}

void __attribute__ ((interrupt ("hypervisor")))
sub3 (void)
{ /* { dg-warning "argument to" } */
}
