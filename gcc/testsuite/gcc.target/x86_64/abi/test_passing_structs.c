/* This tests passing of structs. Only integers are tested.  */

#include "defines.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

struct int_struct
{
  int i;
};

struct long_struct
{
  long l;
};

struct long2_struct
{
  long l1, l2;
};

struct long3_struct
{
  long l1, l2, l3;
};


/* Check that the struct is passed as the individual members in iregs.  */
void
check_struct_passing1 (struct int_struct is ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_struct_passing2 (struct long_struct ls ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_struct_passing3 (struct long2_struct ls ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_struct_passing4 (struct long3_struct ls ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ls.l1 == rsp+8);
  assert ((unsigned long)&ls.l2 == rsp+16);
  assert ((unsigned long)&ls.l3 == rsp+24);
}


int
main (void)
{
  struct int_struct is = { 48 };
  struct long_struct ls = { 49 };
#ifdef CHECK_LARGER_STRUCTS
  struct long2_struct l2s = { 50, 51 };
  struct long3_struct l3s = { 52, 53, 54 };
#endif

  clear_struct_registers;
  iregs.I0 = is.i;
  num_iregs = 1;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing1)(is);

  clear_struct_registers;
  iregs.I0 = ls.l;
  num_iregs = 1;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing2)(ls);

#ifdef CHECK_LARGER_STRUCTS
  clear_struct_registers;
  iregs.I0 = l2s.l1;
  iregs.I1 = l2s.l2;
  num_iregs = 2;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing3)(l2s);
  WRAP_CALL (check_struct_passing4)(l3s);
#endif

  return 0;
}
