#include "defines.h"
#include "macros.h"
#include "args.h"

struct IntegerRegisters iregbits = { ~0, ~0, ~0, ~0, ~0, ~0 };
struct IntegerRegisters iregs;
unsigned int num_iregs;

long long
fun_test_returning_long_long (void)
{
  volatile_var++;
  return (long long) 0xabadbeefabadbeefLL;
}

double
fun_test_returning_double (void)
{
  volatile_var++;
  return (double) 12345678.0;
}

union
{
  long long ll;
  double d;
} test_64;

int
main (void)
{
  unsigned failed = 0;
  long long ll;
  double d;

  clear_struct_registers;
  test_64.ll = 0xabadbeefabadbeefLL;

  ll = WRAP_RET (fun_test_returning_long_long)();
  if (ll != test_64.ll
      || (test_64.ll & 0xffffffff) != eax
      || ((test_64.ll >> 32) & 0xffffffff) != edx)
    failed++;

  clear_struct_registers;
  test_64.d = 12345678.0;

  d = WRAP_RET (fun_test_returning_double)();
  if (d != test_64.d
      || (test_64.ll & 0xffffffff) != eax
      || ((test_64.ll >> 32) & 0xffffffff) != edx)
    __builtin_printf ("fail double\n"), failed++;

  if (failed)
    abort ();
  return 0;
}
