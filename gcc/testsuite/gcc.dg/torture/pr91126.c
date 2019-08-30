/* { dg-do run } */

struct S
{
  __INT32_TYPE__ a : 24;
  __INT32_TYPE__ b : 8;
} s;

int
main()
{
  s.a = 0xfefefe;
  s.b = 0xfe;
  unsigned char c;
  c = ((unsigned char *)&s)[0];
  if (c != 0xfe)
    __builtin_abort ();
  c = ((unsigned char *)&s)[1];
  if (c != 0xfe)
    __builtin_abort ();
  c = ((unsigned char *)&s)[2];
  if (c != 0xfe)
    __builtin_abort ();
  c = ((unsigned char *)&s)[3];
  if (c != 0xfe)
    __builtin_abort ();
  return 0;
}
