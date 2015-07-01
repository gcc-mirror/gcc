/* This tests passing of structs. Only integers are tested.  */

#include "defines.h"
#include "args.h"

struct IntegerRegisters iregbits = { ~0, ~0, ~0, ~0, ~0, ~0 };
struct IntegerRegisters iregs;
unsigned int num_iregs;

struct int_struct
{
  int i;
};

struct longlong_struct
{
  long long ll;
};

struct long2_struct
{
  long long ll1, ll2;
};

struct long3_struct
{
  long l1, l2, l3;
};

union un1
{
  char c;
  int i;
};

union un2
{
  char c1;
  long l;
  char c2;
};

union un3
{
  struct int_struct is;
  struct longlong_struct ls;
  union un1 un;
};


void
check_mixed_passing1 (char c1 ATTRIBUTE_UNUSED, struct int_struct is ATTRIBUTE_UNUSED, char c2 ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_mixed_passing2 (char c1 ATTRIBUTE_UNUSED, struct long3_struct ls ATTRIBUTE_UNUSED, char c2 ATTRIBUTE_UNUSED)
{
  check_int_arguments;

  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ls.l1 == esp+4);
  assert ((unsigned long)&ls.l2 == esp+8);
  assert ((unsigned long)&ls.l3 == esp+12);
}

int
main (void)
{
  struct int_struct is = { 64 };
#ifdef CHECK_LARGER_STRUCTS
  struct long3_struct l3s = { 65, 66, 67 };
#endif

  clear_struct_registers;
  iregs.I0 = 8;
  iregs.I1 = 64;
  iregs.I2 = 9;
  num_iregs = 3;
  clear_int_hardware_registers;
  WRAP_CALL (check_mixed_passing1)(8, is, 9);

#ifdef CHECK_LARGER_STRUCTS
  clear_struct_registers;
  iregs.I0 = 10;
  iregbits.I0 = 0xff;
  iregs.I1 = 11;
  iregbits.I1 = 0xff;
  num_iregs = 2;
  clear_int_hardware_registers;
  WRAP_CALL (check_mixed_passing2)(10, l3s, 11);
#endif

  return 0;
}
