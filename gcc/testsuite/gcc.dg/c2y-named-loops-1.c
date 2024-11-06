/* N3355 - Named loops.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */

void
foo (int w)
{
  d: e: f:;
  a: b: c:
  for (int x = 0; x < 32; ++x)
    {
      if (x == 0)
	continue a;	/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (x == 1)
	continue b;	/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (x == 2)
	continue c;	/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (x == 31)
	break b;	/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    }
  int y = 0;
  g: h:
  #pragma GCC unroll 2
  while (y < 16)
    {
      ++y;
      if (y == 12)
	continue g;	/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (y == 13)
	continue h;	/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (y == 14)
	break g;	/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    }
  i: j:;
  k: l:
  switch (y)
    {
    case 6:
      break;
    case 7:
      break k;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    case 8:
      break l;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
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
		    break ae;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 1)
		    break ab;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 2)
		    break z;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 3)
		    break v;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 4)
		    break s;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 5)
		    break p;		/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
		  else if (w == 6)
		    break;
		  else if (w == 7)
		    continue aa;	/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
		  else if (w == 8)
		    continue y;		/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
		  else if (w == 9)
		    continue x;		/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
		  else if (w == 10)
		    continue m;		/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
		  ag: ah:
		  do
		    {
		      if (w == 11)
			break ag;	/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
		      else
			continue ah;	/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
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
      break ak;				/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    else if (x == 30)
      break aj;				/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    else if (x == 29)
      continue ai;			/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
  al:
  [[]] am:
  [[]]
  do
    {
      if (w == 42)
	continue am;			/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
      else if (w == 41)
	break al;			/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    }
  while (1);
  an:
  [[]] ao:
  [[]] [[]]
  while (w)
    {
      if (w == 40)
	break ao;			/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
      else if (w == 39)
	continue an;			/* { dg-warning "ISO C does not support 'continue' statement with an identifier operand before" } */
    }
  [[]] ap:
  [[]] aq:
  [[]]
  switch (w)
    {
    case 42:
      break ap;				/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    default:
      break aq;				/* { dg-warning "ISO C does not support 'break' statement with an identifier operand before" } */
    }
}
