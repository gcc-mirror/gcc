/* Extracted from gas.  Incorrectly generated non-pic code at -O0 for
   IA-64, which produces linker errors on some operating systems.  */

struct
{
  int offset;
  struct slot
  {
    int field[6];
  }
  slot[4];
} s;

int
x ()
{
  int toggle = 0;
  int r = s.slot[0].field[!toggle];
  return r;
}

int
main ()
{
  return 0;
}
