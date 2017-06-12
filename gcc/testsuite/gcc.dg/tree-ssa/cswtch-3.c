/* PR tree-optimization/79472 */
/* { dg-options "-O2 -fdump-tree-switchconv" } */
/* { dg-do run } */

int *expected;

void
foo (int x, int y)
{
  if (x != expected[0] || y != expected[1])
    __builtin_abort ();
  expected += 2;
}

__attribute__((noinline, noclone)) void
f1 (int v, int w)
{
  int i, j;
  if (w)
    {
      i = 129;
      j = i - 1;
      goto lab;
    }
  switch (v)
    {
    case 170:
      j = 7;
      i = 27;
      break;
    case 171:
      i = 8;
      j = 122;
      break;
    case 172:
      i = 21;
      j = -19;
      break;
    case 173:
      i = 18;
      j = 17;
      break;
    case 174:
      i = 139;
      j = -5;
      break;
    case 175:
      i = 14;
      j = -26;
      break;
    case 176:
      j = 5;
      i = -14;
      break;
    case 177:
      j = 8;
      i = 12;
      break;
    default:
      __builtin_abort ();
    }

 lab:
  foo (i, j);
}

__attribute__((noinline, noclone)) void
f2 (int v)
{
  int i, j;
  switch (v)
    {
    case 170:
      j = 7;
      i = 27;
      break;
    case 171:
      i = 8;
      j = 122;
      break;
    case 172:
      i = 21;
      j = -19;
      break;
    case 173:
      i = 18;
      j = 17;
      break;
    case 174:
      i = 139;
      j = -5;
      break;
    case 175:
      i = 14;
      j = -26;
      break;
    case 176:
      j = 5;
      i = -14;
      break;
    case 177:
      j = 8;
      i = 12;
      break;
    default:
      foo (5, 12);
      foo (17, 19);
      i = 8;
      j = 19;
      break;
    }

  foo (i, j);
}

__attribute__((noinline, noclone)) void
f3 (int v)
{
  int i;
  switch (v)
    {
    default:
      i = v;
      goto lab;
    case 170:
      i = 27;
      break;
    case 171:
      i = 8;
      break;
    case 172:
      i = 21;
      break;
    case 173:
      i = 18;
      break;
    case 174:
      i = 139;
      break;
    case 175:
      i = 14;
      break;
    case 176:
      i = -14;
      break;
    case 177:
      i = 12;
      break;
    }

 lab:
  foo (i, -5);
}

__attribute__((noinline, noclone)) void
f4 (int v, int w)
{
  int i, j, k = 5;
  if (w)
    {
      foo (0, 0);
      k = 26;
      goto do_default;
    }
  switch (v)
    {
    case 170:
      j = 7;
      i = 27;
      break;
    case 171:
      i = 8;
      j = 122;
      break;
    case 172:
      i = 21;
      j = -19;
      break;
    case 173:
      i = 18;
      j = 17;
      break;
    case 174:
      i = 139;
      j = -5;
      break;
    case 175:
      i = 14;
      j = -26;
      break;
    case 176:
      j = 5;
      i = -14;
      break;
    case 177:
      j = 8;
      i = 12;
      break;
    default:
    do_default:
      foo (5, 12);
      foo (17, 19);
      i = 8;
      j = 19;
      break;
    }

  foo (i, j + k);
}

void
f5 (int v, int w)
{
  int i;
  if (w)
    {
      foo (23, 0);
      i = 129;
    }
  else
    switch (v)
      {
      case 170:
	i = 27;
	break;
      case 171:
	i = 8;
	break;
      case 172:
	i = 21;
	break;
      case 173:
	i = 18;
	break;
      case 174:
	i = 139;
	break;
      case 175:
	i = 14;
	break;
      case 176:
	i = -14;
	break;
      case 177:
	i = 12;
	break;
      default:
	i = 80;
	break;
      }

 lab:
  foo (i, 0);
}

int
main ()
{
  int *e;
#define T(call, cnt, ...) \
  expected = e = (int []) __VA_ARGS__;		\
  call;						\
  if (expected != e + cnt)			\
    __builtin_abort ()
  T (f1 (171, 1), 2, { 129, 128 });
  T (f1 (140, 1), 2, { 129, 128 });
  T (f1 (170, 0), 2, { 27, 7 });
  T (f1 (171, 0), 2, { 8, 122 });
  T (f1 (172, 0), 2, { 21, -19 });
  T (f1 (173, 0), 2, { 18, 17 });
  T (f1 (174, 0), 2, { 139, -5 });
  T (f1 (175, 0), 2, { 14, -26 });
  T (f1 (176, 0), 2, { -14, 5 });
  T (f1 (177, 0), 2, { 12, 8 });
  T (f2 (-31), 6, { 5, 12, 17, 19, 8, 19 });
  T (f2 (169), 6, { 5, 12, 17, 19, 8, 19 });
  T (f2 (170), 2, { 27, 7 });
  T (f2 (171), 2, { 8, 122 });
  T (f2 (172), 2, { 21, -19 });
  T (f2 (173), 2, { 18, 17 });
  T (f2 (174), 2, { 139, -5 });
  T (f2 (175), 2, { 14, -26 });
  T (f2 (176), 2, { -14, 5 });
  T (f2 (177), 2, { 12, 8 });
  T (f2 (178), 6, { 5, 12, 17, 19, 8, 19 });
  T (f2 (231), 6, { 5, 12, 17, 19, 8, 19 });
  T (f3 (-31), 2, { -31, -5 });
  T (f3 (169), 2, { 169, -5 });
  T (f3 (170), 2, { 27, -5 });
  T (f3 (171), 2, { 8, -5 });
  T (f3 (172), 2, { 21, -5 });
  T (f3 (173), 2, { 18, -5 });
  T (f3 (174), 2, { 139, -5 });
  T (f3 (175), 2, { 14, -5 });
  T (f3 (176), 2, { -14, -5 });
  T (f3 (177), 2, { 12, -5 });
  T (f3 (178), 2, { 178, -5 });
  T (f3 (231), 2, { 231, -5 });
  T (f4 (171, 1), 8, { 0, 0, 5, 12, 17, 19, 8, 45 });
  T (f4 (140, 1), 8, { 0, 0, 5, 12, 17, 19, 8, 45 });
  T (f4 (-31, 0), 6, { 5, 12, 17, 19, 8, 24 });
  T (f4 (169, 0), 6, { 5, 12, 17, 19, 8, 24 });
  T (f4 (170, 0), 2, { 27, 12 });
  T (f4 (171, 0), 2, { 8, 127 });
  T (f4 (172, 0), 2, { 21, -14 });
  T (f4 (173, 0), 2, { 18, 22 });
  T (f4 (174, 0), 2, { 139, 0 });
  T (f4 (175, 0), 2, { 14, -21 });
  T (f4 (176, 0), 2, { -14, 10 });
  T (f4 (177, 0), 2, { 12, 13 });
  T (f4 (178, 0), 6, { 5, 12, 17, 19, 8, 24 });
  T (f4 (231, 0), 6, { 5, 12, 17, 19, 8, 24 });
  T (f5 (171, 1), 4, { 23, 0, 129, 0 });
  T (f5 (140, 1), 4, { 23, 0, 129, 0 });
  T (f5 (-31, 0), 2, { 80, 0 });
  T (f5 (169, 0), 2, { 80, 0 });
  T (f5 (170, 0), 2, { 27, 0 });
  T (f5 (171, 0), 2, { 8, 0 });
  T (f5 (172, 0), 2, { 21, 0 });
  T (f5 (173, 0), 2, { 18, 0 });
  T (f5 (174, 0), 2, { 139, 0 });
  T (f5 (175, 0), 2, { 14, 0 });
  T (f5 (176, 0), 2, { -14, 0 });
  T (f5 (177, 0), 2, { 12, 0 });
  T (f5 (178, 0), 2, { 80, 0 });
  T (f5 (231, 0), 2, { 80, 0 });
}

/* { dg-final { scan-tree-dump-times "Switch converted" 5 "switchconv" } } */
/* { dg-final { scan-tree-dump-times "= CSWTCH" 8 "switchconv" } } */
