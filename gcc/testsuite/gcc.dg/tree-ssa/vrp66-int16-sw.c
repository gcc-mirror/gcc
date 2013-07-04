/* PR tree-optimization/52267 */
/* { dg-do run { target { int16 } } } */
/* { dg-options "-O2" } */

extern void abort (void);

#define BITSM1 (sizeof (int) * __CHAR_BIT__ - 1)
#define BITSH ((sizeof (int) - sizeof (short)) * __CHAR_BIT__)

void
f1 (unsigned int s)
{
  if ((s & (7U << (BITSM1 - 2))) != 0)
    {
      if (s == (0xc000U << BITSH) - 1 || s == (0xf000U << BITSH) - 1
	  || s == (0x9000U << BITSH) - 1 || s == (0xa031U << BITSH) - 1
	  || s == (0xbfbfU << BITSH) || s == (0xc000U << BITSH)
	  || s == (0xf000U << BITSH) || s == (0x9000U << BITSH)
	  || s == (0xc000U << BITSH) + 1 || s == -1U || s == -15U
	  || s == 0x3cbf || s == 0x3cc0
	  || s == -15550U || s == -15552U || s == (0x7000 << BITSH) - 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == 1U + __INT_MAX__ || s == -32U
	  || s == (3 << (BITSM1 - 2)) + 2 || s == -5U || s == -63U
	  || s == -64U || s == -65U || s == 6U + __INT_MAX__ || s == -8189U
	  || s == -8191U || s == -8192U || s == -8193U || s == -8250U
	  || s == -8255U || s == -8256U || s == -8257U || s == __INT_MAX__
	  || s == __INT_MAX__ + 9U)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3f || s == 1
	  || s == (1 << (BITSM1 - 2)) - 1 || s == 2 || s == 24 || s == 5)
	return;
    }
  abort ();
}

void
f2 (int s)
{
  if ((s & (7U << (BITSM1 - 2))) == 0)
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3f || s == 1
	  || s == (1 << (BITSM1 - 2)) - 1 || s == 2 || s == 24 || s == 5)
	return;
    }
  else
    {
      if (s == (-0x4000 << BITSH) - 1 || s == (-0x1000 << BITSH) - 1
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x5fcf << BITSH) - 1
	  || s == (-0x4041 << BITSH) || s == (-0x4000 << BITSH)
	  || s == (-0x1000 << BITSH) || s == (-0x7000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == -1 || s == -15 || s == -15550
	  || s == 0x3cbf || s == 0x3cc0
	  || s == -15552 || s == (0x7000 << BITSH) - 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == -__INT_MAX__ - 1 || s == -32 || s == (3 << (BITSM1 - 2)) + 2
	  || s == -5 || s == -63 || s == -64 || s == -65
	  || s == -__INT_MAX__ + 4 || s == -8189 || s == -8191 || s == -8192
	  || s == -8193 || s == -8250 || s == -8255 || s == -8256
	  || s == -8257 || s == __INT_MAX__ || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f3 (unsigned int s)
{
  if ((s & 0x3cc0) == 0)
    {
      if (s == 0 || s == 0x20 || s == 0x3f
	  || s == (0xc000U << BITSH)
	  || s == (0xc000U << BITSH) + 1 || s == 1
	  || s == 1U + __INT_MAX__ || s == 2 || s == 24
	  || s == 5 || s == 6U + __INT_MAX__
	  || s == __INT_MAX__ + 9U)
	return;
    }
  else
    {
      if (s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == (0xbfbfU << BITSH) || s == (0xf000U << BITSH)
	  || s == (0x9000U << BITSH)
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (3 << (BITSM1 - 2)) + 2
	  || s == 0x3cbf || s == 0x3cc0 || s == (0xc000U << BITSH) - 1
	  || s == (0xf000U << BITSH) - 1 || s == (0x9000U << BITSH) - 1
	  || s == (0xa031U << BITSH) - 1 || s == -1U || s == -15U
	  || s == -15550U || s == -15552U || s == (0x7000 << BITSH) - 1
	  || s == (1 << (BITSM1 - 2)) - 1 || s == -32U || s == -5U
	  || s == -63U || s == -64U || s == -65U || s == -8189U || s == -8191U
	  || s == -8192U || s == -8193U || s == -8250U || s == -8255U
	  || s == -8256U || s == -8257U || s == __INT_MAX__)
	return;
    }
  abort ();
}

void
f4 (int s)
{
  if ((s & 0x3cc0) == 0)
    {
      if (s == 0 || s == 0x20 || s == 0x3f
	  || s == (-0x4000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == 5 || s == -__INT_MAX__ + 4
	  || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == (-0x4041 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH)
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (3 << (BITSM1 - 2)) + 2
	  || s == 0x3cbf || s == 0x3cc0 || s == (-0x4000 << BITSH) - 1
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == -1 || s == -15 || s == -15550
	  || s == -15552 || s == (0x7000 << BITSH) - 1
	  || s == (1 << (BITSM1 - 2)) - 1 || s == -32 || s == -5 || s == -63
	  || s == -64 || s == -65 || s == -8189 || s == -8191 || s == -8192
	  || s == -8193 || s == -8250 || s == -8255 || s == -8256
	  || s == -8257 || s == __INT_MAX__)
	return;
    }
  abort ();
}

void
f5 (int s)
{
  if ((s & 0x3cc0U) == 0)
    {
      if (s == 0 || s == 0x20 || s == 0x3f
	  || s == (-0x4000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == 5 || s == -__INT_MAX__ + 4
	  || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == (-0x4041 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH)
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (3 << (BITSM1 - 2)) + 2
	  || s == 0x3cbf || s == 0x3cc0 || s == (-0x4000 << BITSH) - 1
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == -1 || s == -15 || s == -15550
	  || s == -15552 || s == (0x7000 << BITSH) - 1
	  || s == (1 << (BITSM1 - 2)) - 1 || s == -32 || s == -5 || s == -63
	  || s == -64 || s == -65 || s == -8189 || s == -8191 || s == -8192
	  || s == -8193 || s == -8250 || s == -8255 || s == -8256
	  || s == -8257 || s == __INT_MAX__)
	return;
    }
  abort ();
}

void
f6 (unsigned int s)
{
  if ((s & 0x3cc0) == 0x3cc0)
    {
      if (s == 0x3cc0 || s == (0xc000U << BITSH) - 1
	  || s == -1U || s == -15U
	  || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32U || s == -5U || s == -63U || s == -64U
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == (0xf000U << BITSH) - 1 || s == (0x9000U << BITSH) - 1
	  || s == (0xa031U << BITSH) - 1
	  || s == (0x7000 << BITSH) - 1
	  || s == 0x20 || s == 0x3cbf || s == 0x3f || s == (0xbfbfU << BITSH)
	  || s == (0xc000U << BITSH) || s == (0xf000U << BITSH)
	  || s == (0x9000U << BITSH) || s == (0xc000U << BITSH) + 1 || s == 1
	  || s == -15550U || s == -15552U || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == 1U + __INT_MAX__ || s == 2
	  || s == 24 || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -65U
	  || s == 6U + __INT_MAX__ || s == -8189U || s == -8191U
	  || s == -8192U || s == -8193U || s == -8250U || s == -8255U
	  || s == -8256U || s == -8257U || s == __INT_MAX__ + 9U)
	return;
    }
  abort ();
}

void
f7 (int s)
{
  if ((s & 0x3cc0) == 0x3cc0)
    {
      if (s == 0x3cc0 || s == (-0x4000 << BITSH) - 1
	  || s == -1 || s == -15
	  || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32 || s == -5 || s == -63 || s == -64 || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1
	  || s == (0x7000 << BITSH) - 1
	  || s == 0x20 || s == 0x3cbf || s == 0x3f || s == (-0x4041 << BITSH)
	  || s == (-0x4000 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -15550 || s == -15552 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == -__INT_MAX__ - 1 || s == 2
	  || s == 24 || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -65
	  || s == -__INT_MAX__ + 4 || s == -8189 || s == -8191 || s == -8192
	  || s == -8193 || s == -8250 || s == -8255 || s == -8256
	  || s == -8257 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f8 (int s)
{
  if ((s & 0x3cc0U) == 0x3cc0)
    {
      if (s == 0x3cc0 || s == (-0x4000 << BITSH) - 1
	  || s == -1 || s == -15
	  || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32 || s == -5 || s == -63 || s == -64 || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1
	  || s == (0x7000 << BITSH) - 1
	  || s == 0x20 || s == 0x3cbf || s == 0x3f || s == (-0x4041 << BITSH)
	  || s == (-0x4000 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -15550 || s == -15552 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == -__INT_MAX__ - 1 || s == 2
	  || s == 24 || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -65
	  || s == -__INT_MAX__ + 4 || s == -8189 || s == -8191 || s == -8192
	  || s == -8193 || s == -8250 || s == -8255 || s == -8256
	  || s == -8257 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f9 (unsigned int s)
{
  if ((s & 0x3cc0) >= 0x1cc0)
    {
      if (s == 0x1cc0 || s == 0x1fff || s == 0x2000 || s == 0x3cbf
	  || s == 0x3cc0 || s == (0xc000U << BITSH) - 1
	  || s == (0xf000U << BITSH) - 1
	  || s == (0xbfbfU << BITSH) || s == (0xf000U << BITSH)
	  || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (3 << (BITSM1 - 2)) + 2
	  || s == (0xa031U << BITSH) - 1 || s == -1U || s == -15U
	  || s == (0x7000 << BITSH) - 1 || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32U || s == -5U || s == -63U || s == -64U || s == -65U
	  || s == -8189U || s == -8191U || s == -8192U || s == -8193U
	  || s == -8250U || s == -8255U || s == -8256U || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x20 || s == 0x3f
	  || s == (0x9000U << BITSH) - 1 || s == (0xc000U << BITSH)
	  || s == (0x9000U << BITSH)
	  || s == (0xc000U << BITSH) + 1 || s == 1 || s == -15550U
	  || s == -15552U
	  || s == 1U + __INT_MAX__ || s == 2
	  || s == 24 || s == 5
	  || s == 6U + __INT_MAX__ || s == -8257U || s == __INT_MAX__ + 9U)
	return;
    }
  abort ();
}

void
f10 (unsigned int s)
{
  if ((s & 0x3cc0) > 0x1cc0)
    {
      if (s == 0x2000 || s == 0x3cbf || s == 0x3cc0
	  || s == (0xc000U << BITSH) - 1 || s == (0xf000U << BITSH) - 1
	  || s == (0xbfbfU << BITSH) || s == (0xa031U << BITSH) - 1
	  || s == (0xf000U << BITSH)
	  || s == -1U || s == -15U || s == (0x7000 << BITSH) - 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == -32U || s == (3 << (BITSM1 - 2)) + 2 || s == -5U
	  || s == -63U || s == -64U || s == -65U || s == -8189U || s == -8191U
	  || s == -8192U || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x20
	  || s == 0x3f || s == (0x9000U << BITSH) - 1
	  || s == (0xc000U << BITSH) || s == (0x9000U << BITSH)
	  || s == (0xc000U << BITSH) + 1 || s == 1 || s == -15550U
	  || s == -15552U || s == (1 << (BITSM1 - 2)) - 1
	  || s == 1U + __INT_MAX__ || s == 2
	  || s == 24 || s == 5
	  || s == 6U + __INT_MAX__ || s == -8193U || s == -8250U
	  || s == -8255U || s == -8256U || s == -8257U
	  || s == __INT_MAX__ + 9U)
	return;
    }
  abort ();
}

void
f11 (int s)
{
  if ((s & 0x3cc0) >= 0x1cc0)
    {
      if (s == 0x1cc0 || s == 0x1fff || s == 0x2000 || s == 0x3cbf
	  || s == 0x3cc0 || s == (-0x4000 << BITSH) - 1
	  || s == (-0x1000 << BITSH) - 1
	  || s == (-0x4041 << BITSH)
	  || s == (-0x1000 << BITSH)
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (3 << (BITSM1 - 2)) + 2
	  || s == (-0x5fcf << BITSH) - 1 || s == -1 || s == -15
	  || s == (0x7000 << BITSH) - 1 || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32 || s == -5 || s == -63 || s == -64 || s == -65
	  || s == -8189 || s == -8191 || s == -8192 || s == -8193
	  || s == -8250 || s == -8255 || s == -8256 || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x20 || s == 0x3f
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x4000 << BITSH)
	  || s == (-0x7000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1 || s == -15550
	  || s == -15552
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == 5 || s == -__INT_MAX__ + 4
	  || s == -8257 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f12 (int s)
{
  if ((s & 0x3cc0) > 0x1cc0)
    {
      if (s == 0x2000 || s == 0x3cbf || s == 0x3cc0
	  || s == (-0x4000 << BITSH) - 1 || s == (-0x1000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1
	  || s == (-0x4041 << BITSH)
	  || s == (-0x1000 << BITSH)
	  || s == -1 || s == -15 || s == (0x7000 << BITSH) - 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (3 << (BITSM1 - 2)) + 2
	  || s == -32 || s == -5 || s == -63
	  || s == -64 || s == -65 || s == -8189 || s == -8191 || s == -8192
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x20
	  || s == 0x3f || s == (-0x7000 << BITSH) - 1
	  || s == (-0x4000 << BITSH) || s == (-0x7000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1 || s == -15550
	  || s == -15552 || s == (1 << (BITSM1 - 2)) - 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == 5 || s == -__INT_MAX__ + 4
	  || s == -8193 || s == -8250 || s == -8255 || s == -8256
	  || s == -8257 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f13 (unsigned int s)
{
  if ((s & (0xe071U << BITSH)) > (0xb030U << BITSH))
    {
      if (s == (0xf000U << BITSH) - 1 || s == (0xc000U << BITSH)
	  || s == (0xf000U << BITSH) || s == (0xc000U << BITSH) + 1
	  || s == -1U || s == -15U || s == -15550U || s == -15552U
	  || s == -32U || s == -5U || s == -63U || s == -64U || s == -65U
	  || s == -8189U || s == -8191U || s == -8192U || s == -8193U
	  || s == -8250U || s == -8255U || s == -8256U || s == -8257U)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (0xc000U << BITSH) - 1 || s == (0x9000U << BITSH) - 1
	  || s == (0xa031U << BITSH) - 1 || s == (0xbfbfU << BITSH)
	  || s == (0x9000U << BITSH) || s == 1 || s == (0x7000 << BITSH) - 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (1 << (BITSM1 - 2)) - 1 || s == 1U + __INT_MAX__ || s == 2
	  || s == 24 || s == (3 << (BITSM1 - 2)) + 2 || s == 5
	  || s == 6U + __INT_MAX__ || s == __INT_MAX__
	  || s == __INT_MAX__ + 9U)
	return;
    }
  abort ();
}

void
f14 (unsigned int s)
{
  if ((s & (0xe071U << BITSH)) > (0xa030U << BITSH))
    {
      if (s == (0xc000U << BITSH) - 1 || s == (0xf000U << BITSH) - 1
	  || s == (0xbfbfU << BITSH) || s == (0xc000U << BITSH)
	  || s == (0xf000U << BITSH) || s == (0xc000U << BITSH) + 1
	  || s == -1U || s == -15U || s == -15550U || s == -15552U
	  || s == -32U || s == -5U || s == -63U || s == -64U || s == -65U
	  || s == -8189U || s == -8191U || s == -8192U || s == -8193U
	  || s == -8250U || s == -8255U || s == -8256U || s == -8257U)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (0x9000U << BITSH) - 1 || s == (0xa031U << BITSH) - 1
	  || s == (0x9000U << BITSH) || s == 1 || s == (0x7000 << BITSH) - 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (1 << (BITSM1 - 2)) - 1 || s == 1U + __INT_MAX__ || s == 2
	  || s == 24 || s == (3 << (BITSM1 - 2)) + 2 || s == 5
	  || s == 6U + __INT_MAX__ || s == __INT_MAX__
	  || s == __INT_MAX__ + 9U)
	return;
    }
  abort ();
}

void
f15 (int s)
{
  if ((s & ((-0x1f8f) << BITSH)) > ((-0x4fd0) << BITSH))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x4000 << BITSH)
	  || s == (-0x1000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -1 || s == -15 || s == -15550 || s == -15552
	  || s == (0x7000 << BITSH) - 1 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == 2 || s == 24 || s == -32 || s == (3 << (BITSM1 - 2)) + 2
	  || s == 5 || s == -5 || s == -63 || s == -64 || s == -65
	  || s == -8189 || s == -8191 || s == -8192 || s == -8193
	  || s == -8250 || s == -8255 || s == -8256 || s == -8257
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == (-0x4000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == (-0x4041 << BITSH)
	  || s == (-0x7000 << BITSH) || s == -__INT_MAX__ - 1
	  || s == -__INT_MAX__ + 4 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f16 (int s)
{
  if ((s & ((-0x1f8f) << BITSH)) >= ((-0x4fd0) << BITSH))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x4000 << BITSH)
	  || s == (-0x1000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -1 || s == -15 || s == -15550 || s == -15552
	  || s == (0x7000 << BITSH) - 1 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == 2 || s == 24 || s == -32 || s == (3 << (BITSM1 - 2)) + 2
	  || s == 5 || s == -5 || s == -63 || s == -64 || s == -65
	  || s == -8189 || s == -8191 || s == -8192 || s == -8193
	  || s == -8250 || s == -8255 || s == -8256 || s == -8257
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == (-0x4000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == (-0x4041 << BITSH)
	  || s == (-0x7000 << BITSH) || s == -__INT_MAX__ - 1
	  || s == -__INT_MAX__ + 4 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f17 (int s)
{
  if ((s & ((-0x4000 << BITSH) | 1)) != -__INT_MAX__ - 1)
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x4000 << BITSH) - 1 || s == (-0x1000 << BITSH) - 1
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x4041 << BITSH)
	  || s == (-0x4000 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1 || s == -1 || s == -15
	  || s == -15550 || s == -15552 || s == (0x7000 << BITSH) - 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == (1 << (BITSM1 - 2)) - 1 || s == 2 || s == 24 || s == -32
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -5 || s == -63
	  || s == -64 || s == -65 || s == -__INT_MAX__ + 4 || s == -8189
	  || s == -8191 || s == -8192 || s == -8193 || s == -8250
	  || s == -8255 || s == -8256 || s == -8257 || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == (-0x5fcf << BITSH) - 1 || s == (-0x7000 << BITSH)
	  || s == -__INT_MAX__ - 1 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f18 (int s)
{
  if ((s & ((-0x4000 << BITSH) | 1)) != ((-0x4000 << BITSH) | 1))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x4000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == (-0x4041 << BITSH)
	  || s == (-0x4000 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH) || s == 1 || s == -15550 || s == -15552
	  || s == (0x7000 << BITSH) - 1 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24 || s == -32
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -64
	  || s == -__INT_MAX__ + 4 || s == -8192 || s == -8250 || s == -8256
	  || s == __INT_MAX__ || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == (-0x1000 << BITSH) - 1 || s == (-0x4000 << BITSH) + 1
	  || s == -1 || s == -15 || s == -5 || s == -63 || s == -65
	  || s == -8189 || s == -8191 || s == -8193 || s == -8255
	  || s == -8257)
	return;
    }
  abort ();
}

void
f19 (int s)
{
  if ((s & ((-0x4000 << BITSH) | 1)) != ((0x4000 << BITSH) | 1))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x4000 << BITSH) - 1 || s == (-0x1000 << BITSH) - 1
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x5fcf << BITSH) - 1
	  || s == (-0x4041 << BITSH) || s == (-0x4000 << BITSH)
	  || s == (-0x1000 << BITSH) || s == (-0x7000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1 || s == -1 || s == -15
	  || s == -15550 || s == -15552 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24 || s == -32
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -5 || s == -63
	  || s == -64 || s == -65 || s == -__INT_MAX__ + 4 || s == -8189
	  || s == -8191 || s == -8192 || s == -8193 || s == -8250
	  || s == -8255 || s == -8256 || s == -8257 || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == (0x7000 << BITSH) - 1 || s == __INT_MAX__)
	return;
    }
  abort ();
}

void
f20 (int s)
{
  if ((s & (-0x1000 << BITSH)) != -__INT_MAX__ - 1)
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x4000 << BITSH) - 1 || s == (-0x1000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == (-0x4041 << BITSH)
	  || s == (-0x4000 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -1 || s == -15 || s == -15550 || s == -15552
	  || s == (0x7000 << BITSH) - 1 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == 2 || s == 24 || s == -32 || s == (3 << (BITSM1 - 2)) + 2
	  || s == 5 || s == -5 || s == -63 || s == -64 || s == -65
	  || s == -8189 || s == -8191 || s == -8192 || s == -8193
	  || s == -8250 || s == -8255 || s == -8256 || s == -8257
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == (-0x7000 << BITSH) - 1 || s == -__INT_MAX__ - 1
	  || s == -__INT_MAX__ + 4 || s == -__INT_MAX__ + 7)
	return;
    }
  abort ();
}

void
f21 (int s)
{
  if ((s & (-0x1000 << BITSH)) != (-0x1000 << BITSH))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x4000 << BITSH) - 1 || s == (-0x1000 << BITSH) - 1
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x5fcf << BITSH) - 1
	  || s == (-0x4041 << BITSH) || s == (-0x4000 << BITSH)
	  || s == (-0x7000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == -15550 || s == -15552
	  || s == (0x7000 << BITSH) - 1 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -__INT_MAX__ + 4
	  || s == -8189 || s == -8191 || s == -8192 || s == -8193
	  || s == -8250 || s == -8255 || s == -8256 || s == -8257
	  || s == __INT_MAX__ || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == (-0x1000 << BITSH) || s == -1 || s == -15
	  || s == -32 || s == -5 || s == -63 || s == -64
	  || s == -65)
	return;
    }
  abort ();
}

void
f22 (int s)
{
  if ((s & (-0x1000 << BITSH)) != (0x7000 << BITSH))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (-0x4000 << BITSH) - 1 || s == (-0x1000 << BITSH) - 1
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x5fcf << BITSH) - 1
	  || s == (-0x4041 << BITSH) || s == (-0x4000 << BITSH)
	  || s == (-0x1000 << BITSH) || s == (-0x7000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1 || s == -1 || s == -15
	  || s == -15550 || s == -15552 || s == (0x7000 << BITSH) - 1
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24 || s == -32
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -5 || s == -63
	  || s == -64 || s == -65 || s == -__INT_MAX__ + 4 || s == -8189
	  || s == -8191 || s == -8192 || s == -8193 || s == -8250
	  || s == -8255 || s == -8256 || s == -8257 || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == (0x7000 << BITSH) || s == __INT_MAX__)
	return;
    }
  abort ();
}

void
f23 (unsigned int s)
{
  if ((s & (0xf000U << BITSH)) != (0x7000 << BITSH))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (0xc000U << BITSH) - 1 || s == (0xf000U << BITSH) - 1
	  || s == (0x9000U << BITSH) - 1 || s == (0xa031U << BITSH) - 1
	  || s == (0xbfbfU << BITSH) || s == (0xc000U << BITSH)
	  || s == (0xf000U << BITSH) || s == (0x9000U << BITSH)
	  || s == (0xc000U << BITSH) + 1 || s == 1 || s == -1U || s == -15U
	  || s == -15550U || s == -15552U || s == (0x7000 << BITSH) - 1
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == 1U + __INT_MAX__ || s == 2 || s == 24 || s == -32U
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -5U || s == -63U
	  || s == -64U || s == -65U || s == 6U + __INT_MAX__ || s == -8189U
	  || s == -8191U || s == -8192U || s == -8193U || s == -8250U
	  || s == -8255U || s == -8256U || s == -8257U
	  || s == __INT_MAX__ + 9U)
	return;
    }
  else
    {
      if (s == (0x7000 << BITSH) || s == __INT_MAX__)
	return;
    }
  abort ();
}

void
f24 (unsigned int s)
{
  if ((s & (0xf000U << BITSH)) != (0x8000U << BITSH))
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f
	  || s == (0xc000U << BITSH) - 1 || s == (0xf000U << BITSH) - 1
	  || s == (0xa031U << BITSH) - 1 || s == (0xbfbfU << BITSH)
	  || s == (0xc000U << BITSH) || s == (0xf000U << BITSH)
	  || s == (0x9000U << BITSH) || s == (0xc000U << BITSH) + 1 || s == 1
	  || s == -1U || s == -15U || s == -15550U || s == -15552U
	  || s == (0x7000 << BITSH) - 1 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == 2 || s == 24 || s == -32U || s == (3 << (BITSM1 - 2)) + 2
	  || s == 5 || s == -5U || s == -63U || s == -64U || s == -65U
	  || s == -8189U || s == -8191U || s == -8192U || s == -8193U
	  || s == -8250U || s == -8255U || s == -8256U || s == -8257U
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == (0x9000U << BITSH) - 1 || s == 1U + __INT_MAX__
	  || s == 6U + __INT_MAX__ || s == __INT_MAX__ + 9U)
	return;
    }
  abort ();
}

/* Make this easier to run on microcontroller architectures by using a switch
   statements instead of arrays.
   We still don't want the compiler to constant-propagate the array contents,
   so use an asm in each accessor function with to hide what's going on.  */

int
svals (int i)
{
  asm ("" : "=g" (i) : "0" (i));
  switch (i)
    {
    case  0: return 0;
    case  1: return 0x1cbf;
    case  2: return 0x1cc0;
    case  3: return 0x1fff;
    case  4: return 0x2000;
    case  5: return 0x20;
    case  6: return 0x3cbf;
    case  7: return 0x3cc0;
    case  8: return 0x3f;
    case  9: return (-0x4000 << BITSH) - 1;
    case 10: return (-0x1000 << BITSH) - 1;
    case 11: return (-0x7000 << BITSH) - 1;
    case 12: return (-0x5fcf << BITSH) - 1;
    case 13: return (-0x4041 << BITSH);
    case 14: return (-0x4000 << BITSH);
    case 15: return (-0x1000 << BITSH);
    case 16: return (-0x7000 << BITSH);
    case 17: return (-0x4000 << BITSH) + 1;
    case 18: return 1;
    case 19: return -1;
    case 20: return -15;
    case 21: return -15550;
    case 22: return -15552;
    case 23: return (0x7000 << BITSH) - 1;
    case 24: return (0x7000 << BITSH);
    case 25: return (1 << (BITSM1 - 2));
    case 26: return (1 << (BITSM1 - 2)) - 1;
    case 27: return -__INT_MAX__ - 1;
    case 28: return 2;
    case 29: return 24;
    case 30: return -32;
    case 31: return (3 << (BITSM1 - 2)) + 2;
    case 32: return 5;
    case 33: return -5;
    case 34: return -63;
    case 35: return -64;
    case 36: return -65;
    case 37: return -__INT_MAX__ + 4;
    case 38: return -8189;
    case 39: return -8191;
    case 40: return -8192;
    case 41: return -8193;
    case 42: return -8250;
    case 43: return -8255;
    case 44: return -8256;
    case 45: return -8257;
    case 46: return __INT_MAX__;
    case 47: return -__INT_MAX__ + 7;
#define SVAL_NUM 48
    default: abort ();
    }
};

unsigned int
uvals (int i)
{
  asm ("" : "=g" (i) : "0" (i));
  switch (i)
    {
    case  0: return 0;
    case  1: return 0x1cbf;
    case  2: return 0x1cc0;
    case  3: return 0x1fff;
    case  4: return 0x2000;
    case  5: return 0x20;
    case  6: return 0x3cbf;
    case  7: return 0x3cc0;
    case  8: return 0x3f;
    case  9: return (0xc000U << BITSH) - 1;
    case 10: return (0xf000U << BITSH) - 1;
    case 11: return (0x9000U << BITSH) - 1;
    case 12: return (0xa031U << BITSH) - 1;
    case 13: return (0xbfbfU << BITSH);
    case 14: return (0xc000U << BITSH);
    case 15: return (0xf000U << BITSH);
    case 16: return (0x9000U << BITSH);
    case 17: return (0xc000U << BITSH) + 1;
    case 18: return 1;
    case 19: return -1U;
    case 20: return -15U;
    case 21: return -15550U;
    case 22: return -15552U;
    case 23: return (0x7000 << BITSH) - 1;
    case 24: return (0x7000 << BITSH);
    case 25: return (1 << (BITSM1 - 2));
    case 26: return (1 << (BITSM1 - 2)) - 1;
    case 27: return 1U + __INT_MAX__;
    case 28: return 2;
    case 29: return 24;
    case 30: return -32U;
    case 31: return (3 << (BITSM1 - 2)) + 2;
    case 32: return 5;
    case 33: return -5U;
    case 34: return -63U;
    case 35: return -64U;
    case 36: return -65U;
    case 37: return 6U + __INT_MAX__;
    case 38: return -8189U;
    case 39: return -8191U;
    case 40: return -8192U;
    case 41: return -8193U;
    case 42: return -8250U;
    case 43: return -8255U;
    case 44: return -8256U;
    case 45: return -8257U;
    case 46: return __INT_MAX__;
    case 47: return __INT_MAX__ + 9U;
#define UVAL_NUM 48
    default: abort ();
    }
};

int
main ()
{
  int i;
  for (i = 0; i < SVAL_NUM; i++)
    {
      f2 (svals (i));
      f4 (svals (i));
      f5 (svals (i));
      f7 (svals (i));
      f8 (svals (i));
      f11 (svals (i));
      f12 (svals (i));
      f15 (svals (i));
      f16 (svals (i));
      f17 (svals (i));
      f18 (svals (i));
      f19 (svals (i));
      f20 (svals (i));
      f21 (svals (i));
      f22 (svals (i));
    }
  for (i = 0; i < UVAL_NUM; i++)
    {
      f1 (uvals (i));
      f3 (uvals (i));
      f6 (uvals (i));
      f9 (uvals (i));
      f10 (uvals (i));
      f13 (uvals (i));
      f14 (uvals (i));
      f23 (uvals (i));
      f24 (uvals (i));
    }
  return 0;
}
