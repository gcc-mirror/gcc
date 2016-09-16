// PR sanitizer/77396
// { dg-do run }
// { dg-set-target-env-var ASAN_OPTIONS "check_initialization_order=true" }

struct S { S () { asm volatile ("" : : : "memory"); } };
static S c;

int
main ()
{
  return 0;
}
