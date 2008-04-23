/* { dg-do compile } */

char *foo() __attribute__((alloc_size(1))); /* { dg-warning "outside range" } */

