/* Test __MQSRAHI.  */
/* { dg-options "-mcpu=fr450" } */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int main ()
{
  if (__MQSRAHI (0x0001000200030004ULL, 1) != 0x0000000100010002ULL)
    abort ();

  if (__MQSRAHI (0xfffffffefffcfff8ULL, 1) != 0xfffffffffffefffcULL)
    abort ();

  if (__MQSRAHI (0x8000c000e000f000ULL, 12) != 0xfff8fffcfffeffffULL)
    abort ();

  if (__MQSRAHI (0x123456789abcdef0ULL, 12) != 0x00010005fff9fffdULL)
    abort ();

  if (__MQSRAHI (0x123456789abcdef0ULL, 16) != 0x123456789abcdef0ULL)
    abort ();

  exit (0);
}
