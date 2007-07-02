/* This is the driver for c_ptr_test_7.  */
extern void abort(void);

void *func0();

int main(int argc, char **argv)
{
  /* The Fortran module c_ptr_tests_7 contains function func0, which has
     return type of c_ptr, and should set the return value to c_null_ptr.  */
  if (func0() != 0)
    abort();

  return 0;
}
