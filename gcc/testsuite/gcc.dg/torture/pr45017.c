/* { dg-do run { target i?86-*-* x86_64-*-* } } */

int tester(char *bytes)
{
  union {
      struct {
	  unsigned int r1:4;
	  unsigned int r2:4;
      } fmt;
      char value[1];
  } ovl;

  ovl.value[0] = bytes[0];
  return ovl.fmt.r1;
}
extern void abort (void);
int main()
{
  char buff = 0x2f;
  if (tester(&buff) != 0x0f)
    abort ();
  return 0;
}

