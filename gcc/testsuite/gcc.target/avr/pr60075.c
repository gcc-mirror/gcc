/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-options "-O2" } */

#define NI __attribute((noipa))
typedef __UINT8_TYPE__ uint8_t;

struct pair { uint8_t data; uint8_t cmd; };
const __flash struct pair table[] =
{
  { 0x11, 1 }, { 0x33, 0 }, { 0x55, 1 }
};

uint8_t cnt;

NI void write_data (uint8_t d)
{
  ++cnt;
  if (cnt == 1 && d == 0x11)
    { /*ok*/ }
  else if (cnt == 3 && d == 0x55)
    { /*ok*/ }
  else
    __builtin_abort ();
}

NI void write_cmd (uint8_t c)
{
  ++cnt;
  if (cnt != 2 || c != 0x33)
    __builtin_abort ();
}

NI void test (uint8_t loops)
{
  for (uint8_t i = 0; i < loops; i++)
    {
      // Bug is reading table[i].cmd from wrong AS.
      if (table[i].cmd)
        write_data (table[i].data);
      else
        write_cmd (table[i].data);
    }
}

int main (void)
{
  test (3);
  return 0;
}
