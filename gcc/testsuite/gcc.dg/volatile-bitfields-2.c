/* { dg-do run } */
/* { dg-options "-fstrict-volatile-bitfields" } */

extern void abort(void);
struct thing {
  volatile unsigned short a: 8;
  volatile unsigned short b: 8;
} t = {1,2};

int main()
{
  t.a = 3;
  if (t.a !=3 || t.b !=2) abort();
  return 0;
}
