/* PR target/56866 */

int
main ()
{
#if __CHAR_BIT__ == 8 && __SIZEOF_LONG_LONG__ == 8 && __SIZEOF_INT__ == 4 && __SIZEOF_SHORT__ == 2
  unsigned long long wq[256], rq[256];
  unsigned int wi[256], ri[256];
  unsigned short ws[256], rs[256];
  unsigned char wc[256], rc[256];
  int t;

  __builtin_memset (wq, 0, sizeof wq);
  __builtin_memset (wi, 0, sizeof wi);
  __builtin_memset (ws, 0, sizeof ws);
  __builtin_memset (wc, 0, sizeof wc);
  wq[0] = 0x0123456789abcdefULL;
  wi[0] = 0x01234567;
  ws[0] = 0x4567;
  wc[0] = 0x73;

  asm volatile ("" : : "g" (wq), "g" (wi), "g" (ws), "g" (wc) : "memory");

  for (t = 0; t < 256; ++t)
    rq[t] = (wq[t] >> 8) | (wq[t] << (sizeof (wq[0]) * __CHAR_BIT__ - 8));
  for (t = 0; t < 256; ++t)
    ri[t] = (wi[t] >> 8) | (wi[t] << (sizeof (wi[0]) * __CHAR_BIT__ - 8));
  for (t = 0; t < 256; ++t)
    rs[t] = (ws[t] >> 9) | (ws[t] << (sizeof (ws[0]) * __CHAR_BIT__ - 9));
  for (t = 0; t < 256; ++t)
    rc[t] = (wc[t] >> 5) | (wc[t] << (sizeof (wc[0]) * __CHAR_BIT__ - 5));

  asm volatile ("" : : "g" (rq), "g" (ri), "g" (rs), "g" (rc) : "memory");

  if (rq[0] != 0xef0123456789abcdULL || rq[1])
    __builtin_abort ();
  if (ri[0] != 0x67012345 || ri[1])
    __builtin_abort ();
  if (rs[0] != 0xb3a2 || rs[1])
    __builtin_abort ();
  if (rc[0] != 0x9b || rc[1])
    __builtin_abort ();
#endif
  return 0;
}
