/* PR middle-end/21850 */

extern void abort (void);

typedef int V2SI __attribute__ ((vector_size (8)));

int
main (void)
{
#if (__INT_MAX__ == 2147483647) \
    && (__LONG_LONG_MAX__ == 9223372036854775807LL)
  if (((int)(long long)(V2SI){ 2, 2 }) != 2)
    abort ();
#endif
  return 0;
}
