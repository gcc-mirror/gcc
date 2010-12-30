// PR debug/45997
// { dg-do compile }
// { dg-options "-gdwarf-2 -dA" }

typedef int my_int;
typedef volatile my_int volatile_my_int;
typedef const volatile_my_int const_volatile_my_int;

my_int v_my_int = 0;
volatile_my_int v_volatile_my_int = 1;
const_volatile_my_int v_const_volatile_my_int = 4;

int
main ()
{
  asm volatile ("" : : "r" (&v_my_int));
  asm volatile ("" : : "r" (&v_volatile_my_int));
  asm volatile ("" : : "r" (&v_const_volatile_my_int));
  return 0;
}

// { dg-final { scan-assembler-times "DIE\[^\n\r\]*DW_TAG_base_type" 1 } }
