  /* These tests require double precision, so for hosts that don't offer
     that much precision, just ignore these test.  */
  if (sizeof (double) >= 8) {
    if (d2u(0.0) != 0)
      abort();
    if (d2u(0.999) != 0)
      abort();
    if (d2u(1.0) != 1)
      abort();
    if (d2u(1.99) != 1)
      abort();
    if (d2u((double) (~0U)) != ~0U)			/* 0xffffffff */
      abort();
    if (d2u((double) ((~0U) >> 1)) != (~0U) >> 1)		/* 0x7fffffff */
      abort();
    if (d2u((double) ~((~0U) >> 1)) != ~((~0U) >> 1))	/* 0x80000000 */
      abort();
  }
  /* These tests require double precision, so for hosts that don't offer
     that much precision, just ignore these test.  */
  if (sizeof (double) >= 8) {
    if (d2s(0.0) != 0)
      abort();
    if (d2s(0.999) != 0)
      abort();
    if (d2s(1.0) != 1)
      abort();
    if (d2s(1.99) != 1)
      abort();
    if (d2s(-0.999) != 0)
      abort();
    if (d2s(-1.0) != -1)
      abort();
    if (d2s(-1.99) != -1)
      abort();
    if (d2s((double) ((~0U) >> 1)) != (~0U) >> 1)		/* 0x7fffffff */
      abort();
    if (d2s((double)(int)~((~0U) >> 1)) != (int)~((~0U) >> 1)) /* 0x80000000 */
      abort();
  }
