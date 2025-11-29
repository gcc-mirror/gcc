// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile }
// { dg-skip-if "" { *-*-* } { "-ftrivial-auto-var-init=*" } { "" } }
// { dg-options "-O2 -fdump-tree-gimple" }
// All the s1..s24 variables and i1 need .DEFERRED_INIT call on their
// declarations.
// Plus, forward gotos to l1 & l2 labels need up to s1-s4 and s6-s9 vars to
// be .DEFERRED_INITed (and backward gotos up to that minus the first two).
// switch to case 15 skips over s12, switch to case 16/17 skip
// over s12 and s13 but the adjacent l3 label needs to also skip over s3-s4
// and s6-s9 and s11.  switch to case 18 skips over s12-s14 and switch to
// default in the same switch skips over s12-s15.
// goto l4; skips over s19 initialization.
// goto l5; skips over s20-s22 initialization.
// switch to case 32/33 skips over s23 but goto to adjacent l6 skips also
// over s20-s22.  switch to default in that switch skips over s23-s24.
// { dg-final { scan-tree-dump-times "  s1 = \.DEFERRED_INIT \\\(" 2 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s2 = \.DEFERRED_INIT \\\(" 2 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s3 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s4 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s5 = \.DEFERRED_INIT \\\(" 1 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s6 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s7 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s8 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s9 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s10 = \.DEFERRED_INIT \\\(" 1 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s11 = \.DEFERRED_INIT \\\(" 2 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s12 = \.DEFERRED_INIT \\\(" 5 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s13 = \.DEFERRED_INIT \\\(" 4 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s14 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s15 = \.DEFERRED_INIT \\\(" 2 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s16 = \.DEFERRED_INIT \\\(" 1 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s17 = \.DEFERRED_INIT \\\(" 1 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s18 = \.DEFERRED_INIT \\\(" 1 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s19 = \.DEFERRED_INIT \\\(" 2 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s20 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s21 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s22 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s23 = \.DEFERRED_INIT \\\(" 3 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  s24 = \.DEFERRED_INIT \\\(" 2 "gimple" { target c++26 } } }
// { dg-final { scan-tree-dump-times "  i1 = \.DEFERRED_INIT \\\(" 1 "gimple" { target c++26 } } }

struct S { int a, b, c; };

template <typename S>
int
foo (int x)
{
  int r = 0;
  if (x == 1)
    goto l1;
  S s1;
  if (x == 2)
    goto l1;
  S s2;
  {
    S s10;
    if (x == 12)
      goto l1;
    s10.a = 1;
    r += s10.a;
    int i1;
    if (x == 13)
      goto l1;
    i1 = 2;
    r += i1;
  }
  if (x == 3)
    goto l2;
  if (x == 4)
    goto l1;
  {
    S s3;
    if (x == 5)
      goto l2;
    S s4;
    if (x == 6)
      goto l1;
    {
      S s5;
      if (x == 7)
	goto l1;
      s5.a = 5;
      r += s5.a;
    }
    S s6;
    {
      S s7;
      S s8;
      if (x == 8)
	goto l1;
      S s9;
      if (x == 9)
	goto l2;
      if (x == 10)
	goto l2;
      if (x == 11)
	goto l2;
      l1:
      l2:
      s1.a = 1;
      s2.b = 2;
      s3.c = 3;
      s4.a = 4;
      s6.b = 6;
      s7.c = 7;
      s8.a = 8;
      s9.b = 9;
      r += s1.a + s2.b + s3.c;
      r += s4.a + s6.b + s7.c;
      r += s8.a + s9.b;
      if (x == 14)
	goto l3;
      S s11;
      switch (x)
	{
	  S s12;
	case 15:
	  S s13;
	  // FALLTHRU
	l3:
	case 16:
	case 17:
	  S s14;
	  s11.a = 1;
	  s12.b = 2;
	  s13.c = 3;
	  s14.a = 4;
	  r += s11.a + s12.b + s13.c;
	  r += s14.a;
	  return r;
	case 18:
	  S s15;
	  s11.a = 1;
	  s12.b = 2;
	  s13.c = 3;
	  s14.a = 4;
	  s15.b = 5;
	  r += s11.a + s12.b + s13.c;
	  r += s14.a + s15.b;
	  return r;
	default:
	  if (x != 19 && x != 20)
	    break;
	  S s16;
	  s11.a = 1;
	  s12.b = 2;
	  s13.c = 3;
	  s14.a = 4;
	  s15.b = 5;
	  s16.c = 6;
	  r += s11.a + s12.b + s13.c;
	  r += s14.a + s15.b + s16.c;
	  return r;
	}
      if (x == 21)
	goto l3;
    }
    S s17;
    if (x == 22)
      goto l3;
    if (x == 23)
      goto l1;
    if (x == 24)
      goto l2;
    s17.a = 1;
    r += s17.a;
  }
  S s18;
  if (x == 25)
    {
      S s19;
      s19.c = 2;
      r += s19.c;
      if (x == 29)
	l4:;
      goto l3;
    }
  if (x == 26)
    goto l1;
  if (x == 27)
    goto l2;
  s18.b = 1;
  r += s18.b;
  if (x == 28)
    goto l4;
  {
    S s20;
    {
      S s21;
      if (x == 29)
	goto l1;
      S s22;
      if (x == 30)
	goto l2;
      l5:
      s20.a = 1;
      s21.b = 2;
      s22.c = 3;
      r += s20.a + s21.b + s22.c;
      switch (x)
	{
	case 31:
	  S s23;
	  // FALLTHRU
	l6:
	case 32:
	case 33:
	  S s24;
	  s23.a = 1;
	  s24.b = 2;
	  r += s23.a + s24.b;
	  return r;
	default:
	  if (x >= 34 && x <= 35)
	    return r;
	  break;
	}
      if (x == 34)
	goto l5;
      if (x == 35)
	goto l6;
      return r;
    }
    if (x == 36)
      goto l5;
    if (x == 37)
      goto l6;
  }
  if (x == 38)
    goto l5;
  if (x == 39)
    goto l6;
  return r;
}

int
bar (int x)
{
  return foo <S> (x);
}
