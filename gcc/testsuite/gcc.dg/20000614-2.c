/* { dg-do run { target i?86-*-* } } */
/* { dg-options "-O2 -fno-strength-reduce" } */

extern void abort (void);
extern void exit (int);

char buf[8];

void bar(char *p)
{
}

int main()
{
  union {
    unsigned int val;
    unsigned char p[4];
  } serial;

  int i;
  serial.val = 0;
  bar(buf);
  for(i = 0; i < 8; i += 4)
    {
      serial.p [0] += buf [i + 0];
      serial.p [1] += buf [i + 1];
      serial.p [2] += buf [i + 2];
      serial.p [3] += buf [i + 3];
    }
  if (serial.val)
    abort();
  exit(0);
}
