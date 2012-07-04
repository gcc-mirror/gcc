// { dg-do compile }
// { dg-options "" }

int
main (int argc, char **argv)
{
  typedef char A[argc];
  new A; // { dg-warning "variable-length array types|not a constant" }
  new A[0]; // { dg-error "must be constant|not a constant" }
  new A[5]; // { dg-error "must be constant|not a constant" }
  new (A[0]); // { dg-error "must be constant|not a constant" }
  new (A[5]); // { dg-error "must be constant|not a constant" }
}
