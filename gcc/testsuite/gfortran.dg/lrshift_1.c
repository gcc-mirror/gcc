/* Left and right shift C routines, to compare to Fortran results.  */
int c_lshift_ (int *x, int *y) { return (*x) << (*y); }
int c_rshift_ (int *x, int *y) { return (*x) >> (*y); }
