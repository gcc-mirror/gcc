/* This tests passing of structs. */

#include "defines.h"
#include "args.h"
#include <complex.h>

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

struct longlong2_struct
{
  long long ll1, ll2;
};

struct longlong3_struct
{
  long long ll1, ll2, ll3;
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
check_struct_passing3 (struct longlong2_struct ls ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_struct_passing4 (struct longlong3_struct ls ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ls.ll1 == esp+4);
  assert ((unsigned long)&ls.ll2 == esp+12);
  assert ((unsigned long)&ls.ll3 == esp+20);
}

struct flex1_struct
{
  long i;
  long flex[];
};

struct flex2_struct
{
  long i;
  long flex[0];
};

void
check_struct_passing7 (struct flex1_struct is ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

void
check_struct_passing8 (struct flex2_struct is ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

struct complex1_struct
{
  __complex__ float x;
};

struct complex1a_struct
{
  long l;
  union
    {
      float f;
      int i;
    } u;
};

void
check_struct_passing9 (struct complex1_struct is ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

struct long3_struct
{
  long l1, l2, l3;
};

void
check_struct_passing10 (struct long3_struct ls ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ls.l1 == esp+4);
  assert ((unsigned long)&ls.l2 == esp+8);
  assert ((unsigned long)&ls.l3 == esp+12);
}

struct char3_struct
{
  char c1, c2, c3;
};

void
check_struct_passing11 (struct char3_struct is ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

struct char7_struct
{
  char c1, c2, c3, c4, c5, c6, c7;
};

void
check_struct_passing12 (struct char7_struct is ATTRIBUTE_UNUSED)
{
  check_int_arguments;
}

static struct flex1_struct f1s = { 60, { } };
static struct flex2_struct f2s = { 61, { } };

int
main (void)
{
  struct int_struct is = { 48 };
  struct long_struct ls = { 49 };
#ifdef CHECK_LARGER_STRUCTS
  struct longlong2_struct ll2s = { 50, 51 };
  struct longlong3_struct ll3s = { 52, 53, 54 };
  struct long3_struct l3s = { 60, 61, 62 };
#endif
  struct complex1_struct c1s = { ( -13.4 + 3.5*I ) };
  union
    {
      struct complex1_struct c;
      struct complex1a_struct u;
    } c1u;
  struct char3_struct c3 = { 0x12, 0x34, 0x56 };
  union
    {
      struct char3_struct c;
      int i;
    } c3u;
  struct char7_struct c7 = { 0x12, 0x34, 0x56, 0x78, 0x12, 0x34, 0x56 };
  union
    {
      struct char7_struct c;
      struct
	{
	  int i0, i1;
	} i;
    } c7u;

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
  num_iregs = 0;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing3)(ll2s);
  WRAP_CALL (check_struct_passing4)(ll3s);
  WRAP_CALL (check_struct_passing10)(l3s);
#endif

  clear_struct_registers;
  iregs.I0 = f1s.i;
  num_iregs = 1;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing7)(f1s);

  clear_struct_registers;
  iregs.I0 = f2s.i;
  num_iregs = 1;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing8)(f2s);

  clear_struct_registers;
  c1u.c = c1s;
  iregs.I0 = c1u.u.l;
  iregs.I1 = c1u.u.u.i;
  num_iregs = 2;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing9)(c1s);

  clear_struct_registers;
  c3u.c = c3;
  iregs.I0 = c3u.i;
  iregbits.I0 = 0xffffff;
  num_iregs = 1;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing11)(c3);

  clear_struct_registers;
  c7u.c = c7;
  iregs.I0 = c7u.i.i0;
  iregs.I1 = c7u.i.i1;
  iregbits.I0 = 0xffffffff;
  iregbits.I1 = 0xffffff;
  num_iregs = 2;
  clear_int_hardware_registers;
  WRAP_CALL (check_struct_passing12)(c7);

  return 0;
}
