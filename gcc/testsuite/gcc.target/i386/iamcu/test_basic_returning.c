#include "defines.h"
#include "macros.h"
#include "args.h"

char
fun_test_returning_char (void)
{
  volatile_var++;
  return 64;
}

short
fun_test_returning_short (void)
{
  volatile_var++;
  return 65;
}

int
fun_test_returning_int (void)
{
  volatile_var++;
  return 66;
}

long
fun_test_returning_long (void)
{
  volatile_var++;
  return 67;
}

float
fun_test_returning_float (void)
{
  volatile_var++;
  return 68;
}

#define def_test_returning_type(fun, type, ret, reg) \
  { type var = WRAP_RET (fun) (); \
    union { type r; unsigned long reg; } u; \
    u.reg = reg; \
  assert (ret == u.r && ret == var); }

int
main (void)
{
  def_test_returning_type(fun_test_returning_char, char, 64, eax);
  def_test_returning_type(fun_test_returning_short, short, 65, eax);
  def_test_returning_type(fun_test_returning_int, int, 66, eax);
  def_test_returning_type(fun_test_returning_long, long, 67, eax);
  def_test_returning_type(fun_test_returning_float, float, 68, eax);
  return 0;
}
