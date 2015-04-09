/* { dg-do run } */
/* { dg-require-effective-target size32plus } */
/* { dg-options "-fstrict-volatile-bitfields" } */

struct s
{
  char x : 8;
  unsigned int y : 31;
} __attribute__((packed));

volatile struct s global;

int
main ()
{
  global.y = 0x7FFFFFFF;
  if (global.y != 0x7FFFFFFF)
    __builtin_abort ();
  return 0;
}
