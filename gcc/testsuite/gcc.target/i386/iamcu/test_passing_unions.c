/* This tests passing of structs.  */

#include "defines.h"
#include "args.h"

struct IntegerRegisters iregbits = { ~0, ~0, ~0, ~0, ~0, ~0 };
struct IntegerRegisters iregs;
unsigned int num_iregs;

struct int_struct
{
  int i;
};

struct long_struct
{
  long l;
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
  struct long_struct ls;
  union un1 un;
};


void
check_union_passing1(union un1 u ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_union_passing2(union un2 u1 ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_union_passing3(union un3 u ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

#define check_union_passing1 WRAP_CALL(check_union_passing1)
#define check_union_passing2 WRAP_CALL(check_union_passing2)
#define check_union_passing3 WRAP_CALL(check_union_passing3)

union un4
{
  int i;
  float f;
};

union un5
{
  long long ll;
  double d;
};

void
check_union_passing4(union un4 u1 ATTRIBUTE_UNUSED,
		     union un4 u2 ATTRIBUTE_UNUSED,
		     union un4 u3 ATTRIBUTE_UNUSED,
		     union un4 u4 ATTRIBUTE_UNUSED,
		     union un4 u5 ATTRIBUTE_UNUSED,
		     union un4 u6 ATTRIBUTE_UNUSED,
		     union un4 u7 ATTRIBUTE_UNUSED,
		     union un4 u8 ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_union_passing5(union un5 u ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

#define check_union_passing4 WRAP_CALL(check_union_passing4)
#define check_union_passing5 WRAP_CALL(check_union_passing5)

#ifdef CHECK_FLOAT128
union un6
{
  __float128 f128;
  int i;
};


void
check_union_passing6(union un6 u ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u.f128 == esp+4);
  assert ((unsigned long)&u.i == esp+4);
}

#define check_union_passing6 WRAP_CALL(check_union_passing6)
#endif

int
main (void)
{
  union un1 u1;
#ifdef CHECK_LARGER_UNION_PASSING
  union un2 u2;
  union un3 u3;
  struct int_struct is;
  struct long_struct ls;
#endif /* CHECK_LARGER_UNION_PASSING */
  union un4 u4[8];
  union un5 u5;
  int i;
#ifdef CHECK_FLOAT128
  union un6 u6;
#endif

  /* Check a union with char, int.  */
  clear_struct_registers;
  u1.i = 0;  /* clear the struct to not have high bits left */
  u1.c = 32;
  iregs.I0 = 32;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing1(u1);
  u1.i = 0;  /* clear the struct to not have high bits left */
  u1.i = 33;
  iregs.I0 = 33;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing1(u1);

  /* Check a union with char, long, char.  */
#ifdef CHECK_LARGER_UNION_PASSING
  clear_struct_registers;
  u2.l = 0;  /* clear the struct to not have high bits left */
  u2.c1 = 34;
  iregs.I0 = 34;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing2(u2);
  u2.l = 0;  /* clear the struct to not have high bits left */
  u2.l = 35;
  iregs.I0 = 35;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing2(u2);
  u2.l = 0;  /* clear the struct to not have high bits left */
  u2.c2 = 36;
  iregs.I0 = 36;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing2(u2);

  /* check a union containing two structs and a union.  */
  clear_struct_registers;
  is.i = 37;
  u3.ls.l = 0;  /* clear the struct to not have high bits left */
  u3.is = is;
  iregs.I0 = 37;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing3(u3);
  ls.l = 38;
  u3.ls.l = 0;  /* clear the struct to not have high bits left */
  u3.ls = ls;
  iregs.I0 = 38;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing3(u3);
  u1.c = 39;
  u3.ls.l = 0;  /* clear the struct to not have high bits left */
  u3.un = u1;
  iregs.I0 = 39;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing3(u3);
  u1.i = 40;
  u3.ls.l = 0;  /* clear the struct to not have high bits left */
  u3.un = u1;
  iregs.I0 = 40;
  num_iregs = 1;
  clear_int_hardware_registers;
  check_union_passing3(u3);
#endif /* CHECK_LARGER_UNION_PASSING */

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    u4[i].f = 32 + i;
  iregs.I0 = u4[0].i;
  iregs.I1 = u4[1].i;
  iregs.I2 = u4[2].i;
  num_iregs = 3;
  clear_int_hardware_registers;
  check_union_passing4(u4[0], u4[1], u4[2], u4[3],
		       u4[4], u4[5], u4[6], u4[7]);

  clear_struct_registers;
  u5.d = 48.394;
  iregs.I0 = u5.ll & 0xffffffff;
  iregs.I1 = (u5.ll >> 32) & 0xffffffff;
  num_iregs = 2;
  clear_int_hardware_registers;
  check_union_passing5(u5);

#ifdef CHECK_FLOAT128
  u6.i = 2;
  check_union_passing6(u6);
#endif

  return 0;
}
