struct a { char *b; } c[D] /* { dg-error "undeclared" } */
  =                        /* { dg-error "storage size" } */
    { { "" } } ;  /* { dg-warning "braces around scalar initializer|near" } */
