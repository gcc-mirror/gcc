struct a { char *b; } c[D] /* { dg-error "undeclared" } */
  = { { "" } } ;  /* { dg-warning "braces around scalar initializer|near" } */
