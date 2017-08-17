/* PR middle-end/81814 */
/* { dg-do run } */

int
main ()
{
  int i = 0x01000000;
  int a;

  a = ((signed char) i) != 0 ? 0 : (unsigned long long int) i;
  if (a != 0x01000000)
    __builtin_abort ();
  a = ((signed short int) i) != 0 ? 0 : (unsigned long long int) i;
  if (a != 0x01000000)
    __builtin_abort ();
  a = ((unsigned short int) i) != 0 ? 0 : (unsigned long long int) i;
  if (a != 0x01000000)
    __builtin_abort ();
  a = ((unsigned char) i) != 0 ? 0 : (unsigned long long int) i;
  if (a != 0x01000000)
    __builtin_abort ();
  a = ((signed char) i) == 0 ? (unsigned long long int) i : 0;
  if (a != 0x01000000)
    __builtin_abort ();
  a = ((signed short int) i) == 0 ? (unsigned long long int) i : 0;
  if (a != 0x01000000)
    __builtin_abort ();
  a = ((unsigned short int) i) == 0 ? (unsigned long long int) i : 0;
  if (a != 0x01000000)
    __builtin_abort ();
  a = ((unsigned char) i) == 0 ? (unsigned long long int) i : 0;
  if (a != 0x01000000)
    __builtin_abort ();

  return 0;
}
