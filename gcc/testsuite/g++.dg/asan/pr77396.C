// PR sanitizer/77396
// { dg-do run }
// { dg-set-target-env-var ASAN_OPTIONS "check_initialization_order=true" }

static int a = 0; 
static int b = a; 

int
main ()
{
  return 0;
}
