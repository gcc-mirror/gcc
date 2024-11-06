/* N3355 - Named loops.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

void
foo (int w)
{
  d: e: f:;
  a: b: c:
  for (int x = 0; x < 32; ++x)
    {
      if (x == 0)
	continue a;	/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (x == 1)
	continue b;	/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (x == 2)
	continue c;	/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (x == 31)
	break b;	/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    }
  int y = 0;
  g: h:
  #pragma GCC unroll 2
  while (y < 16)
    {
      ++y;
      if (y == 12)
	continue g;	/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (y == 13)
	continue h;	/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (y == 14)
	break g;	/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    }
  i: j:;
  k: l:
  switch (y)
    {
    case 6:
      break;
    case 7:
      break k;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    case 8:
      break l;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    }
  m: n: o: p:
  for (int x = 0; x < 2; ++x)
    q: r: s: t:
    switch (x)
      {
      case 0:
	u: v:
      case 3:
	w: x:
	for (int y = 0; y < 2; ++y)
	  y: z:
	  for (int z = 0; z < 2; ++z)
	    aa: ab: ac:
	    for (int a = 0; a < 2; ++a)
	      ad: ae: af:
	      switch (a)
		{
		case 0:
		  if (w == 0)
		    break ae;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 1)
		    break ab;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 2)
		    break z;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 3)
		    break v;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 4)
		    break s;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 5)
		    break p;		/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 6)
		    break;
		  else if (w == 7)
		    continue aa;	/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
		  else if (w == 8)
		    continue y;		/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
		  else if (w == 9)
		    continue x;		/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
		  else if (w == 10)
		    continue m;		/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
		  ag: ah:
		  do
		    {
		      if (w == 11)
			break ag;	/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
		      else
			continue ah;	/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
		    }
		  while (0);
		  break;
		default:
		  break;
		}
	break;
      default:
	break;
      }
  [[]] [[]] ai:
  [[]] [[]] aj:
  [[]] [[]] ak:
  [[]] [[]] [[]]
  for (int x = 0; x < 32; ++x)
    if (x == 31)
      break ak;				/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    else if (x == 30)
      break aj;				/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    else if (x == 29)
      continue ai;			/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
  al:
  [[]] am:
  [[]]
  do
    {
      if (w == 42)
	continue am;			/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (w == 41)
	break al;			/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    }
  while (1);
  an:
  [[]] ao:
  [[]] [[]]
  while (w)
    {
      if (w == 40)
	break ao;			/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
      else if (w == 39)
	continue an;			/* { dg-error "ISO C does not support 'continue' statement with an identifier operand before" } */
    }
  [[]] ap:
  [[]] aq:
  [[]]
  switch (w)
    {
    case 42:
      break ap;				/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    default:
      break aq;				/* { dg-error "ISO C does not support 'break' statement with an identifier operand before" } */
    }
}
