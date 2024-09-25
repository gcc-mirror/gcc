/* { dg-do run } */
/* { dg-options "-Wl,--defsym,test6_xdata=0" } */

#ifdef __AVR_TINY__
#define PM __attribute__((__progmem__))
#else
/* On general core, just resort to vanilla C. */
#define PM /* Empty */
#endif

#define PSTR(s) (__extension__({ static const char __c[] PM = (s); &__c[0];}))

#define NI __attribute__((noipa))

const volatile int data[] PM = { 1234, 5678 };
const volatile int * volatile pdata = &data[1];

int ram[2];

const int myvar PM = 42;
extern const int xvar __asm ("myvar") PM;

NI int const volatile* get_addr_1 (void)
{
  return &data[1];
}

NI int const volatile* get_addr_x (int x)
{
  return &data[x];
}

void test_1 (void)
{
  if (data[0] != 1234)
    __builtin_abort();

  if (data[1] != 5678)
    __builtin_abort();
}

void test_2 (void)
{
  if (data[1] != 5678)
    __builtin_abort();
}

void test_3 (void)
{
  if (&data[1] != pdata)
    __builtin_abort();
}

void test_4 (void)
{
  if (5678 != *get_addr_1())
    __builtin_abort();
  if (5678 != *get_addr_x(1))
    __builtin_abort();
}

void test_5 (void)
{
  __builtin_memcpy (&ram, (void*) &data, 4);
  if (ram[0] - ram[1] != 1234 - 5678)
    __builtin_abort();
}

const char pmSTR[] PM = "01234";

NI const char* get_pmSTR (int i)
{
  return pmSTR + 2 + i;
}

void test_6 (void)
{
#ifdef __AVR_TINY__
  extern const int test6_xdata PM;
  const char* str = PSTR ("Hallo");
  if (0 == (__AVR_TINY_PM_BASE_ADDRESS__ & (__UINTPTR_TYPE__) str))
    __builtin_abort();
  if (0 == (__AVR_TINY_PM_BASE_ADDRESS__ & (__UINTPTR_TYPE__) test6_xdata))
    __builtin_abort();
#endif
  
  if (get_pmSTR (0)[0] != '0' + 2)
    __builtin_abort();
  if (get_pmSTR (1)[0] != '1' + 2)
    __builtin_abort();
}

void test_7 (void)
{
  if (xvar != 42)
    __builtin_abort();
}

int main()
{
  test_1();
  test_2();
  test_3();
  test_4();
  test_5();
  test_6();
  test_7();
  return 0;
}
