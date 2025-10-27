// PR middle-end/121389
// { dg-do compile { target musttail } }
// { dg-options "-fsanitize=address" }

int foo (void);
int bar (void);
int baz (unsigned *);

int
bar (void)
{
  for (int a = 0; a < 420; ++a)
    {
      for (int b = 0; b < 420; ++b)
	{
	  for (int c = 0; c < 420; ++c)
	    {
	      unsigned t;
	      int u = baz (&t);
	      if (u == 42)
		[[gnu::musttail]] return foo ();
	      if (u == -42)
		break;
	      if (u == 16)
		goto l1;
	      if (u == 18)
		goto l2;
	      if (u == 20)
		goto l3;
	      switch (u)
		{
		case 100: goto l100;
		case 101: goto l101;
		case 102: goto l102;
		case 103: goto l103;
		case 104: goto l104;
		case 105: goto l105;
		case 106: goto l106;
		case 107: goto l107;
		case 108: goto l108;
		case 109: goto l109;
		case 110: goto l110;
		case 111: goto l111;
		case 112: goto l112;
		case 113: goto l113;
		case 114: goto l114;
		case 115: goto l115;
		case 116: goto l116;
		case 117: goto l117;
		case 118: goto l118;
		case 119: goto l119;
		case 120: goto l120;
		case 121: goto l121;
		case 122: goto l122;
		case 123: goto l123;
		case 124: goto l124;
		case 125: goto l125;
		case 126: goto l126;
		case 127: goto l127;
		case 128: goto l128;
		case 129: goto l129;
		}
	    }
	  l3:;
	  foo ();
	  l100:
	  foo ();
	  l101:
	  foo ();
	  l102:
	  foo ();
	  l103:
	  foo ();
	  l104:
	  foo ();
	  l105:
	  foo ();
	  l106:
	  foo ();
	  l107:
	  foo ();
	  l108:
	  foo ();
	  l109:;
	}
      l2:;
      foo ();
      l110:
      foo ();
      l111:
      foo ();
      l112:
      foo ();
      l113:
      foo ();
      l114:
      foo ();
      l115:
      foo ();
      l116:
      foo ();
      l117:
      foo ();
      l118:
      foo ();
      l119:;
    }
  l1:;
  foo ();
  l120:
  foo ();
  l121:
  foo ();
  l122:
  foo ();
  l123:
  foo ();
  l124:
  foo ();
  l125:
  foo ();
  l126:
  foo ();
  l127:
  foo ();
  l128:
  foo ();
  l129:;
  return 42;
}
