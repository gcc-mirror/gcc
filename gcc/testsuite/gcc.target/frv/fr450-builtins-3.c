/* Test __MQSLLHI.  */
/* { dg-options "-mcpu=fr450" } */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int main ()
{
  if (__MQSLLHI (0x0001000200030004ULL, 1) != 0x0002000400060008ULL)
    abort ();

  if (__MQSLLHI (0xfffffffefffcfff8ULL, 1) != 0xfffefffcfff8fff0ULL)
    abort ();

  if (__MQSLLHI (0xfffffffefffcfff8ULL, 12) != 0xf000e000c0008000ULL)
    abort ();

  if (__MQSLLHI (0x123456789abcdef0ULL, 12) != 0x40008000c0000000ULL)
    abort ();

  if (__MQSLLHI (0x123456789abcdef0ULL, 16) != 0x123456789abcdef0ULL)
    abort ();

  exit (0);
}
