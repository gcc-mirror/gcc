/* PR middle-end/102403 - ICE in init_from_control_deps, at
   gimple-predicate-analysis.cc:2364
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

int __fmaf (void)
{
  int a = 0;
  int b, c, d, e, f;

  int r = 0;

  switch (b)        // { dg-warning "-Wuninitialized" }
    {
    default:
      c |= 1;

    case 0:
      if (c == 0)
	a = 1;

      switch (d) {
      case 15:
	f = c;
	break;

      case 11:
      case 6:
      case 4:
	f = c;
      case 10:
	e = a;
      }

      if (e == 0)   // { dg-warning "-Wmaybe-uninitialized" }
  	f = 0;

      r = f;
  }

  // The return statement below gets the unhelpful warning:
  // 'f' may be used uninitialized in this function [-Wmaybe-uninitialized]
  return r;
}

/* Prune out warnings issued on the wrong lines, such as:
   uninit-pr102403.c:9:13: warning: ‘d’ is used uninitialized [-Wuninitialized]
   { dg-prune-output "-Wuninitialized" }
   { dg-prune-output "-Wmaybe-uninitialized" } */
