/* PR tree-optimization/52267 */
/* { dg-do run { target { ! int16 } } } */
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
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f || s == 1
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
	  || s == 0x20 || s == 0x3cbf || s == 0x3cc0 || s == 0x3f || s == 1
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
      if (s == 0 || s == 0x20 || s == 0x3f || s == (0xbfbfU << BITSH)
	  || s == (0xc000U << BITSH) || s == (0xf000U << BITSH)
	  || s == (0x9000U << BITSH) || s == (0xc000U << BITSH) + 1 || s == 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == 1U + __INT_MAX__ || s == 2 || s == 24
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == 6U + __INT_MAX__
	  || s == __INT_MAX__ + 9U)
	return;
    }
  else
    {
      if (s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
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
      if (s == 0 || s == 0x20 || s == 0x3f || s == (-0x4041 << BITSH)
	  || s == (-0x4000 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -__INT_MAX__ + 4
	  || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
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
      if (s == 0 || s == 0x20 || s == 0x3f || s == (-0x4041 << BITSH)
	  || s == (-0x4000 << BITSH) || s == (-0x1000 << BITSH)
	  || s == (-0x7000 << BITSH) || s == (-0x4000 << BITSH) + 1 || s == 1
	  || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -__INT_MAX__ + 4
	  || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
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
	  || s == (0xf000U << BITSH) - 1 || s == (0x9000U << BITSH) - 1
	  || s == (0xa031U << BITSH) - 1 || s == -1U || s == -15U
	  || s == (0x7000 << BITSH) - 1 || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32U || s == -5U || s == -63U || s == -64U
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
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
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == -1 || s == -15
	  || s == (0x7000 << BITSH) - 1 || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32 || s == -5 || s == -63 || s == -64 || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
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
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
	  || s == (-0x5fcf << BITSH) - 1 || s == -1 || s == -15
	  || s == (0x7000 << BITSH) - 1 || s == (1 << (BITSM1 - 2)) - 1
	  || s == -32 || s == -5 || s == -63 || s == -64 || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x2000
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
	  || s == (0xf000U << BITSH) - 1 || s == (0x9000U << BITSH) - 1
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
	  || s == (0xbfbfU << BITSH) || s == (0xc000U << BITSH)
	  || s == (0xf000U << BITSH) || s == (0x9000U << BITSH)
	  || s == (0xc000U << BITSH) + 1 || s == 1 || s == -15550U
	  || s == -15552U || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == 1U + __INT_MAX__ || s == 2
	  || s == 24 || s == (3 << (BITSM1 - 2)) + 2 || s == 5
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
	  || s == (0x9000U << BITSH) - 1 || s == (0xa031U << BITSH) - 1
	  || s == -1U || s == -15U || s == (0x7000 << BITSH) - 1
	  || s == (1 << (BITSM1 - 2)) - 1 || s == -32U || s == -5U
	  || s == -63U || s == -64U || s == -65U || s == -8189U || s == -8191U
	  || s == -8192U || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x20
	  || s == 0x3f || s == (0xbfbfU << BITSH) || s == (0xc000U << BITSH)
	  || s == (0xf000U << BITSH) || s == (0x9000U << BITSH)
	  || s == (0xc000U << BITSH) + 1 || s == 1 || s == -15550U
	  || s == -15552U || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == 1U + __INT_MAX__ || s == 2
	  || s == 24 || s == (3 << (BITSM1 - 2)) + 2 || s == 5
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
	  || s == (-0x1000 << BITSH) - 1 || s == (-0x7000 << BITSH) - 1
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
	  || s == (-0x4041 << BITSH) || s == (-0x4000 << BITSH)
	  || s == (-0x1000 << BITSH) || s == (-0x7000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1 || s == -15550
	  || s == -15552 || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -__INT_MAX__ + 4
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
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x5fcf << BITSH) - 1
	  || s == -1 || s == -15 || s == (0x7000 << BITSH) - 1
	  || s == (1 << (BITSM1 - 2)) - 1 || s == -32 || s == -5 || s == -63
	  || s == -64 || s == -65 || s == -8189 || s == -8191 || s == -8192
	  || s == __INT_MAX__)
	return;
    }
  else
    {
      if (s == 0 || s == 0x1cbf || s == 0x1cc0 || s == 0x1fff || s == 0x20
	  || s == 0x3f || s == (-0x4041 << BITSH) || s == (-0x4000 << BITSH)
	  || s == (-0x1000 << BITSH) || s == (-0x7000 << BITSH)
	  || s == (-0x4000 << BITSH) + 1 || s == 1 || s == -15550
	  || s == -15552 || s == (0x7000 << BITSH) || s == (1 << (BITSM1 - 2))
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -__INT_MAX__ + 4
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
	  || s == (-0x7000 << BITSH) - 1 || s == (-0x5fcf << BITSH) - 1
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
      if (s == (-0x4041 << BITSH) || s == (-0x7000 << BITSH)
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
	  || s == (0x7000 << BITSH) - 1 || s == (0x7000 << BITSH)
	  || s == (1 << (BITSM1 - 2)) || s == (1 << (BITSM1 - 2)) - 1
	  || s == -__INT_MAX__ - 1 || s == 2 || s == 24
	  || s == (3 << (BITSM1 - 2)) + 2 || s == 5 || s == -__INT_MAX__ + 4
	  || s == __INT_MAX__ || s == -__INT_MAX__ + 7)
	return;
    }
  else
    {
      if (s == (-0x1000 << BITSH) || s == -1 || s == -15 || s == -15550
	  || s == -15552 || s == -32 || s == -5 || s == -63 || s == -64
	  || s == -65 || s == -8189 || s == -8191 || s == -8192 || s == -8193
	  || s == -8250 || s == -8255 || s == -8256 || s == -8257)
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

int svals[] = {
  0,
  0x1cbf,
  0x1cc0,
  0x1fff,
  0x2000,
  0x20,
  0x3cbf,
  0x3cc0,
  0x3f,
  (-0x4000 << BITSH) - 1,
  (-0x1000 << BITSH) - 1,
  (-0x7000 << BITSH) - 1,
  (-0x5fcf << BITSH) - 1,
  (-0x4041 << BITSH),
  (-0x4000 << BITSH),
  (-0x1000 << BITSH),
  (-0x7000 << BITSH),
  (-0x4000 << BITSH) + 1,
  1,
  -1,
  -15,
  -15550,
  -15552,
  (0x7000 << BITSH) - 1,
  (0x7000 << BITSH),
  (1 << (BITSM1 - 2)),
  (1 << (BITSM1 - 2)) - 1,
  -__INT_MAX__ - 1,
  2,
  24,
  -32,
  (3 << (BITSM1 - 2)) + 2,
  5,
  -5,
  -63,
  -64,
  -65,
  -__INT_MAX__ + 4,
  -8189,
  -8191,
  -8192,
  -8193,
  -8250,
  -8255,
  -8256,
  -8257,
  __INT_MAX__,
  -__INT_MAX__ + 7,
};

unsigned int uvals[] = {
  0,
  0x1cbf,
  0x1cc0,
  0x1fff,
  0x2000,
  0x20,
  0x3cbf,
  0x3cc0,
  0x3f,
  (0xc000U << BITSH) - 1,
  (0xf000U << BITSH) - 1,
  (0x9000U << BITSH) - 1,
  (0xa031U << BITSH) - 1,
  (0xbfbfU << BITSH),
  (0xc000U << BITSH),
  (0xf000U << BITSH),
  (0x9000U << BITSH),
  (0xc000U << BITSH) + 1,
  1,
  -1U,
  -15U,
  -15550U,
  -15552U,
  (0x7000 << BITSH) - 1,
  (0x7000 << BITSH),
  (1 << (BITSM1 - 2)),
  (1 << (BITSM1 - 2)) - 1,
  1U + __INT_MAX__,
  2,
  24,
  -32U,
  (3 << (BITSM1 - 2)) + 2,
  5,
  -5U,
  -63U,
  -64U,
  -65U,
  6U + __INT_MAX__,
  -8189U,
  -8191U,
  -8192U,
  -8193U,
  -8250U,
  -8255U,
  -8256U,
  -8257U,
  __INT_MAX__,
  __INT_MAX__ + 9U,
};

int
main ()
{
  int i;
  for (i = 0; i < sizeof (svals) / sizeof (svals[0]); i++)
    {
      f2 (svals[i]);
      f4 (svals[i]);
      f5 (svals[i]);
      f7 (svals[i]);
      f8 (svals[i]);
      f11 (svals[i]);
      f12 (svals[i]);
      f15 (svals[i]);
      f16 (svals[i]);
      f17 (svals[i]);
      f18 (svals[i]);
      f19 (svals[i]);
      f20 (svals[i]);
      f21 (svals[i]);
      f22 (svals[i]);
    }
  for (i = 0; i < sizeof (uvals) / sizeof (uvals[0]); i++)
    {
      f1 (uvals[i]);
      f3 (uvals[i]);
      f6 (uvals[i]);
      f9 (uvals[i]);
      f10 (uvals[i]);
      f13 (uvals[i]);
      f14 (uvals[i]);
      f23 (uvals[i]);
      f24 (uvals[i]);
    }
  return 0;
}
