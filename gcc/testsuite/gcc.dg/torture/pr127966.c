/* { dg-do run } */

int main()
{
  int i = 0;
  while (((unsigned char *)"\x80\x80")[i] == 128)
    i++;
  if (i != 2)
    __builtin_abort ();
  return 0;
}
