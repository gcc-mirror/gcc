/* PR target/86048 */
/* { dg-do assemble { target sse2 } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target return_address } */

extern void abort (void);

void *foo (unsigned int *data, unsigned int len)
{
  unsigned int local_data[128];

  if (len > 128)
    abort ();

  for (unsigned int i = 0; i < len; i++)
    local_data[i] = data[i] + data[len - 1 - i] * 2;

  void *ret = __builtin_frame_address (0);

  for (unsigned int i = 0; i < len; i++)
    ret = ret + local_data[i] % 8;

  __asm__ __volatile__ ("" : : : "%xmm6");

  return ret;
}
