/* Procedure pointer test. Used by proc_ptr_7.f90.
   PR fortran/32580.  */

int f(void) {
  return 42;
}

void assignf_(int(**ptr)(void)) {
  *ptr = f;
}
