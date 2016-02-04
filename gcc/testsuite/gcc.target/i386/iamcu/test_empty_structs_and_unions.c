/* This tests passing and returning of empty structures and unions.  */

#include "defines.h"
#include "args.h"

struct IntegerRegisters iregbits = { ~0, ~0, ~0, ~0, ~0, ~0 };
struct IntegerRegisters iregs;
unsigned int num_iregs;

struct empty_struct
{
};

struct empty_struct
check_struct_passing(struct empty_struct s0 ATTRIBUTE_UNUSED,
		     struct empty_struct s1 ATTRIBUTE_UNUSED,
		     int i0 ATTRIBUTE_UNUSED)
{
  struct empty_struct s;
  check_int_arguments;
  return s;
}

#define check_struct_passing WRAP_CALL(check_struct_passing)

union empty_union
{
};

union empty_union
check_union_passing(union empty_union u0 ATTRIBUTE_UNUSED,
		    union empty_union u1 ATTRIBUTE_UNUSED,
		    int i0 ATTRIBUTE_UNUSED)
{
  union empty_union u;
  check_int_arguments;
  return u;
}

#define check_union_passing WRAP_CALL(check_union_passing)

int
main (void)
{
  struct empty_struct s;
  union empty_union u;

  clear_struct_registers;
  iregs.I0 = 32;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing(u,u,32);

  clear_struct_registers;
  iregs.I0 = 33;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_struct_passing(s,s,33);

  return 0;
}
