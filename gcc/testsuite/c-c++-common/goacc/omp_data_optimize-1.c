/* Test 'gcc/omp-data-optimize.c'.  */

/* { dg-additional-options "-fdump-tree-gimple-raw" } */
/* { dg-additional-options "-fopt-info-omp-all" } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_compute[variable c_compute 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_compute } to avoid
   "WARNING: dg-line var l_compute defined, but not used".
   { dg-line l_use[variable c_use 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_use } to avoid
   "WARNING: dg-line var l_use defined, but not used".
   { dg-line l_lcf[variable c_lcf 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_lcf } to avoid
   "WARNING: dg-line var l_lcf defined, but not used".  */

extern int ef1(int);


/* Optimization happens.  */

long opt_1_gvar1;
extern short opt_1_evar1;
static long opt_1_svar1;

static int opt_1(int opt_1_pvar1)
{
  int opt_1_lvar1;
  extern short opt_1_evar2;
  static long opt_1_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    int dummy1 = opt_1_pvar1;
    int dummy2 = opt_1_lvar1;
    int dummy3 = opt_1_evar2;
    int dummy4 = opt_1_svar2;

    int dummy5 = opt_1_gvar1;
    int dummy6 = opt_1_evar1;
    int dummy7 = opt_1_svar1;
  }

  return 0;

/* { dg-optimized {'map\(force_tofrom:opt_1_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_1_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:opt_1_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_1_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_1_evar2 \[len: [0-9]+\]\)' not optimized: opt_1_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_1_svar2 \[len: [0-9]+\]\)' not optimized: opt_1_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:opt_1_gvar1 \[len: [0-9]+\]\)' not optimized: opt_1_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_1_evar1 \[len: [0-9]+\]\)' not optimized: opt_1_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_1_svar1 \[len: [0-9]+\]\)' not optimized: opt_1_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

long opt_2_gvar1;
extern short opt_2_evar1;
static long opt_2_svar1;

static int opt_2(int opt_2_pvar1)
{
  int opt_2_lvar1;
  extern short opt_2_evar2;
  static long opt_2_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    int dummy1 = opt_2_pvar1;
    int dummy2 = opt_2_lvar1;
    int dummy3 = opt_2_evar2;
    int dummy4 = opt_2_svar2;

    int dummy5 = opt_2_gvar1;
    int dummy6 = opt_2_evar1;
    int dummy7 = opt_2_svar1;
  }

  /* A write does not inhibit optimization.  */

  opt_2_pvar1 = 0;
  opt_2_lvar1 = 1;
  opt_2_evar2 = 2;
  opt_2_svar2 = 3;

  opt_2_gvar1 = 10;
  opt_2_evar1 = 11;
  opt_2_svar1 = 12;

  return 0;

/* { dg-optimized {'map\(force_tofrom:opt_2_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_2_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:opt_2_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_2_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_2_evar2 \[len: [0-9]+\]\)' not optimized: opt_2_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_2_svar2 \[len: [0-9]+\]\)' not optimized: opt_2_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_2_gvar1 \[len: [0-9]+\]\)' not optimized: opt_2_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {'map\(force_tofrom:opt_2_evar1 \[len: [0-9]+\]\)' not optimized: opt_2_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {'map\(force_tofrom:opt_2_svar1 \[len: [0-9]+\]\)' not optimized: opt_2_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

long opt_3_gvar1;
extern short opt_3_evar1;
static long opt_3_svar1;

static int opt_3(int opt_3_pvar1)
{
  int opt_3_lvar1;
  extern short opt_3_evar2;
  static long opt_3_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    /* A write inside the kernel inhibits optimization to firstprivate.
       TODO: optimize to private where the variable is dead-on-entry.  */

    opt_3_pvar1 = 1;
    opt_3_lvar1 = 2;
    opt_3_evar2 = 3;
    opt_3_svar2 = 4;

    opt_3_gvar1 = 5;
    opt_3_evar1 = 6;
    opt_3_svar1 = 7;
  }

  return 0;

/* { dg-optimized {'map\(force_tofrom:opt_3_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_3_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:opt_3_pvar1 \[len: [0-9]+\]\)' further optimized to 'private\(opt_3_pvar1\)'} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-optimized {'map\(force_tofrom:opt_3_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_3_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:opt_3_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(opt_3_lvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_3_evar2 \[len: [0-9]+\]\)' not optimized: opt_3_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:opt_3_svar2 \[len: [0-9]+\]\)' not optimized: opt_3_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:opt_3_gvar1 \[len: [0-9]+\]\)' not optimized: opt_3_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {'map\(force_tofrom:opt_3_evar1 \[len: [0-9]+\]\)' not optimized: opt_3_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {'map\(force_tofrom:opt_3_svar1 \[len: [0-9]+\]\)' not optimized: opt_3_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static void opt_4()
{
  int opt_4_larray1[10];

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      int dummy1 = opt_4_larray1[4];
      int dummy2 = opt_4_larray1[8];
    }

/* { dg-optimized {'map\(tofrom:opt_4_larray1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_4_larray1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-bogus {'map\(to:opt_4_larray1 \[len: [0-9]+\]\)' further optimized to 'firstprivate\(opt_4_larray1\)'} "" { target *-*-* } l_compute$c_compute }  */
}

static void opt_5 (int opt_5_pvar1)
{
  int opt_5_larray1[10];

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      opt_5_larray1[opt_5_pvar1] = 1;
      opt_5_pvar1[opt_5_larray1] = 2;
    }

/* { dg-optimized {'map\(force_tofrom:opt_5_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_5_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */

/* TODO: this probably should be optimizable.  */
/* { dg-missed {'map\(tofrom:opt_5_larray1 \[len: [0-9]+\]\)' not optimized: opt_5_larray1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}


/* Similar, but with optimization inhibited because of variable use.  */

static int use_1(int use_1_pvar1)
{
  float use_1_lvar1;
  extern char use_1_evar2;
  static double use_1_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    use_1_pvar1 = 0;
    use_1_lvar1 = 1;
    use_1_evar2 = 2;
    use_1_svar2 = 3;
  }

  int s = 0;
  s += use_1_pvar1; /* { dg-missed {\.\.\. here} "" { target *-*-* } } */
  s += use_1_lvar1; /* { dg-missed {\.\.\. here} "" { target *-*-* } } */
  s += use_1_evar2; /* { dg-bogus {note: \.\.\. here} "" { target *-*-* } }  */
  s += use_1_svar2; /* { dg-bogus {note: \.\.\. here} "" { target *-*-* } }  */

  return s;

/* { dg-missed {'map\(force_tofrom:use_1_pvar1 \[len: [0-9]+\]\)' not optimized: use_1_pvar1 used\.\.\.} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:use_1_lvar1 \[len: [0-9]+\]\)' not optimized: use_1_lvar1 used\.\.\.} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:use_1_evar2 \[len: [0-9]+\]\)' not optimized: use_1_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:use_1_svar2 \[len: [0-9]+\]\)' not optimized: use_1_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

extern int use_2_a1[];

static int use_2(int use_2_pvar1)
{
  int use_2_lvar1;
  extern int use_2_evar2;
  static int use_2_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    use_2_pvar1 = 0;
    use_2_lvar1 = 1;
    use_2_evar2 = 2;
    use_2_svar2 = 3;
  }

  int s = 0;
  s += use_2_a1[use_2_pvar1]; /* { dg-missed {\.\.\. here} "" { target *-*-* } } */
  s += use_2_a1[use_2_lvar1]; /* { dg-missed {\.\.\. here} "" { target *-*-* } } */
  s += use_2_a1[use_2_evar2];
  s += use_2_a1[use_2_svar2];

  return s;

/*TODO The following GIMPLE dump scanning maybe too fragile (across
  different GCC configurations)?  The idea is to verify that we're indeed
  doing the "deep scanning", as discussed in
  <http://mid.mail-archive.com/877dm463sc.fsf@euler.schwinge.homeip.net>.  */
/* { dg-final { scan-tree-dump-times {(?n)  gimple_assign <array_ref, [^,]+, use_2_a1\[use_2_pvar1\], NULL, NULL>$} 1 "gimple" } } */
/* { dg-missed {'map\(force_tofrom:use_2_pvar1 \[len: [0-9]+\]\)' not optimized: use_2_pvar1 used\.\.\.} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-final { scan-tree-dump-times {(?n)  gimple_assign <array_ref, [^,]+, use_2_a1\[use_2_lvar1\], NULL, NULL>$} 1 "gimple" } } */
/* { dg-missed {'map\(force_tofrom:use_2_lvar1 \[len: [0-9]+\]\)' not optimized: use_2_lvar1 used\.\.\.} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-final { scan-tree-dump-times {(?n)  gimple_assign <var_decl, use_2_evar2\.[^,]+, use_2_evar2, NULL, NULL>$} 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times {(?n)  gimple_assign <array_ref, [^,]+, use_2_a1\[use_2_evar2\.[^\]]+\], NULL, NULL>$} 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times {(?n)  gimple_assign <var_decl, use_2_svar2\.[^,]+, use_2_svar2, NULL, NULL>$} 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times {(?n)  gimple_assign <array_ref, [^,]+, use_2_a1\[use_2_svar2\.[^\]]+\], NULL, NULL>$} 1 "gimple" } } */
/* { dg-missed {'map\(force_tofrom:use_2_evar2 \[len: [0-9]+\]\)' not optimized: use_2_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:use_2_svar2 \[len: [0-9]+\]\)' not optimized: use_2_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute } */
}

static void use_3 ()
{
  int use_5_lvar1;
  int use_5_larray1[10];

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      use_5_lvar1 = 5;
    }

  use_5_larray1[use_5_lvar1] = 1; /* { dg-line l_use[incr c_use] } */

/* { dg-missed {'map\(force_tofrom:use_5_lvar1 \[len: [0-9]+\]\)' not optimized: use_5_lvar1 used\.\.\.} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use } */
}


/* Similar, but with the optimization inhibited because of looping/control flow.  */

static void lcf_1(int lcf_1_pvar1)
{
  float lcf_1_lvar1;
  extern char lcf_1_evar2;
  static double lcf_1_svar2;

  for (int i = 0; i < ef1(i); ++i) /* { dg-line l_lcf[incr c_lcf] } */
 {
#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_1_pvar1 = 0;
    lcf_1_lvar1 = 1;
    lcf_1_evar2 = 2;
    lcf_1_svar2 = 3;
  }
 }

/* { dg-missed {'map\(force_tofrom:lcf_1_evar2 \[len: [0-9]+\]\)' not optimized: lcf_1_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_1_svar2 \[len: [0-9]+\]\)' not optimized: lcf_1_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_1_pvar1 \[len: [0-9]+\]\)' not optimized: lcf_1_pvar1 disguised by looping/control flow\.\.\.} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:lcf_1_lvar1 \[len: [0-9]+\]\)' not optimized: lcf_1_lvar1 disguised by looping/control flow\.\.\.} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_lcf$c_lcf } */
}

static void lcf_2(int lcf_2_pvar1)
{
  float lcf_2_lvar1;
  extern char lcf_2_evar2;
  static double lcf_2_svar2;

  if (ef1 (0))
    return;

 repeat:
#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_2_pvar1 = 0;
    lcf_2_lvar1 = 1;
    lcf_2_evar2 = 2;
    lcf_2_svar2 = 3;
  }

  goto repeat; /* { dg-line l_lcf[incr c_lcf] } */

/* { dg-missed {'map\(force_tofrom:lcf_2_evar2 \[len: [0-9]+\]\)' not optimized: lcf_2_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_2_svar2 \[len: [0-9]+\]\)' not optimized: lcf_2_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_2_pvar1 \[len: [0-9]+\]\)' not optimized: lcf_2_pvar1 disguised by looping/control flow\.\.\.} "" { target *-*-* } l_compute$c_compute }
/* { dg-missed {'map\(force_tofrom:lcf_2_lvar1 \[len: [0-9]+\]\)' not optimized: lcf_2_lvar1 disguised by looping/control flow\.\.\.} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_lcf$c_lcf } */
}

static void lcf_3(int lcf_3_pvar1)
{
  float lcf_3_lvar1;
  extern char lcf_3_evar2;
  static double lcf_3_svar2;

  if (ef1 (0))
    return;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_3_pvar1 = 0;
    lcf_3_lvar1 = 1;
    lcf_3_evar2 = 2;
    lcf_3_svar2 = 3;
  }

  // Backward jump after kernel
 repeat:
  goto repeat; /* { dg-line l_lcf[incr c_lcf] } */

/* { dg-missed {'map\(force_tofrom:lcf_3_evar2 \[len: [0-9]+\]\)' not optimized: lcf_3_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_3_svar2 \[len: [0-9]+\]\)' not optimized: lcf_3_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_3_pvar1 \[len: [0-9]+\]\)' not optimized: lcf_3_pvar1 disguised by looping/control flow\.\.\.} "" { target *-*-* } l_compute$c_compute }
/* { dg-missed {'map\(force_tofrom:lcf_3_lvar1 \[len: [0-9]+\]\)' not optimized: lcf_3_lvar1 disguised by looping/control flow\.\.\.} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_lcf$c_lcf } */
}

static void lcf_4(int lcf_4_pvar1)
{
  float lcf_4_lvar1;
  extern char lcf_4_evar2;
  static double lcf_4_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_4_pvar1 = 0;
    lcf_4_lvar1 = 1;
    lcf_4_evar2 = 2;
    lcf_4_svar2 = 3;
  }

  // Forward jump after kernel
  goto out;

    out:
  return;

/* { dg-missed {'map\(force_tofrom:lcf_4_evar2 \[len: [0-9]+\]\)' not optimized: lcf_4_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_4_pvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_4_pvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:lcf_4_svar2 \[len: [0-9]+\]\)' not optimized: lcf_4_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_4_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_4_lvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:lcf_4_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_4_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:lcf_4_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_4_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */
}

static void lcf_5(int lcf_5_pvar1)
{
  float lcf_5_lvar1;
  extern char lcf_5_evar2;
  static double lcf_5_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_5_pvar1 = 0;
    lcf_5_lvar1 = 1;
    lcf_5_evar2 = 2;
    lcf_5_svar2 = 3;
  }

  if (ef1 (-1))
    ;

  return;

/* { dg-optimized {'map\(force_tofrom:lcf_5_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_5_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_5_pvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_5_pvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:lcf_5_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_5_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_5_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_5_lvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:lcf_5_evar2 \[len: [0-9]+\]\)' not optimized: lcf_5_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_5_svar2 \[len: [0-9]+\]\)' not optimized: lcf_5_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static void lcf_6(int lcf_6_pvar1)
{
  float lcf_6_lvar1;
  extern char lcf_6_evar2;
  static double lcf_6_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_6_pvar1 = 0;
    lcf_6_lvar1 = 1;
    lcf_6_evar2 = 2;
    lcf_6_svar2 = 3;
  }

  int x = ef1 (-2) ? 1 : -1;

  return;

/* { dg-optimized {'map\(force_tofrom:lcf_6_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_6_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_6_pvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_6_pvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:lcf_6_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_6_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_6_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_6_lvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:lcf_6_evar2 \[len: [0-9]+\]\)' not optimized: lcf_6_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_6_svar2 \[len: [0-9]+\]\)' not optimized: lcf_6_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static void lcf_7(int lcf_7_pvar1)
{
  float lcf_7_lvar1;
  extern char lcf_7_evar2;
  static double lcf_7_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_7_pvar1 = 0;
    lcf_7_lvar1 = 1;
    lcf_7_evar2 = 2;
    lcf_7_svar2 = 3;
  }

  switch (ef1 (-2))
    {
    case 0: ef1 (10); break;
    case 2: ef1 (11); break;
    default: ef1 (12); break;
    }

  return;

/* { dg-optimized {'map\(force_tofrom:lcf_7_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_7_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_7_pvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_7_pvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:lcf_7_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_7_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_7_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_7_lvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:lcf_7_evar2 \[len: [0-9]+\]\)' not optimized: lcf_7_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_7_svar2 \[len: [0-9]+\]\)' not optimized: lcf_7_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static void lcf_8(int lcf_8_pvar1)
{
  float lcf_8_lvar1;
  extern char lcf_8_evar2;
  static double lcf_8_svar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  {
    lcf_8_pvar1 = 0;
    lcf_8_lvar1 = 1;
    lcf_8_evar2 = 2;
    lcf_8_svar2 = 3;
  }

  asm goto ("" :::: out);

out:
  return;

/* { dg-optimized {'map\(force_tofrom:lcf_8_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_8_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_8_pvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_8_pvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:lcf_8_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_8_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:lcf_8_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_8_lvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {'map\(force_tofrom:lcf_8_evar2 \[len: [0-9]+\]\)' not optimized: lcf_8_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:lcf_8_svar2 \[len: [0-9]+\]\)' not optimized: lcf_8_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

/* Ensure that variables are promoted to private properly.  */

static void priv_1 ()
{
  int priv_1_lvar1, priv_1_lvar2, priv_1_lvar3, priv_1_lvar4, priv_1_lvar5;
  int priv_1_lvar6, priv_1_lvar7, priv_1_lvar8, priv_1_lvar9, priv_1_lvar10;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      priv_1_lvar1 = 1;
      int dummy = priv_1_lvar2;

      if (priv_1_lvar2)
	{
	  priv_1_lvar3 = 1;
	}
      else
	{
	  priv_1_lvar3 = 2;
	}

      priv_1_lvar5 = priv_1_lvar3;

      if (priv_1_lvar2)
	{
	  priv_1_lvar4 = 1;
	  int dummy = priv_1_lvar4;
	}

      switch (priv_1_lvar2)
	{
	case 0:
	  priv_1_lvar5 = 1;
	  dummy = priv_1_lvar6;
	  break;
	case 1:
	  priv_1_lvar5 = 2;
	  priv_1_lvar6 = 3;
	  break;
	default:
	  break;
	}

      asm goto ("" :: "r"(priv_1_lvar7) :: label1, label2);
      if (0)
	{
label1:
	  priv_1_lvar8 = 1;
	  priv_1_lvar9 = 2;
	}
      if (0)
	{
label2:
	  dummy = priv_1_lvar9;
	  dummy = priv_1_lvar10;
	}
    }

/* { dg-optimized {'map\(force_tofrom:priv_1_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-optimized {'map\(to:priv_1_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar1\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar2 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar2 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-bogus {'map\(to:priv_1_lvar2 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar2\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar3 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar3 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-optimized {'map\(to:priv_1_lvar3 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar3\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar4 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar4 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-optimized {'map\(to:priv_1_lvar4 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar4\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar5 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar5 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-optimized {'map\(to:priv_1_lvar5 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar5\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar6 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar6 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-bogus {'map\(to:priv_1_lvar6 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar6\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar7 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar7 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-bogus {'map\(to:priv_1_lvar7 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar7\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar8 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar8 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-optimized {'map\(to:priv_1_lvar8 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar8\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar9 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar9 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-bogus {'map\(to:priv_1_lvar9 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar9\)'} "" { target *-*-* } l_compute$c_compute } */
/* { dg-optimized {'map\(force_tofrom:priv_1_lvar10 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar10 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
 { dg-bogus {'map\(to:priv_1_lvar10 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar10\)'} "" { target *-*-* } l_compute$c_compute } */
}

static void multiple_kernels_1 ()
{
#pragma acc kernels
    {
      int multiple_kernels_1_lvar1 = 1;
    }

    int multiple_kernels_2_lvar1;
#pragma acc kernels
    {
      int multiple_kernels_2_lvar1 = 1;
    }

#pragma acc parallel
    {
      multiple_kernels_2_lvar1++;
    }
}

static int ref_1 ()
{
  int *ref_1_ref1;
  int ref_1_lvar1;

  ref_1_ref1 = &ref_1_lvar1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      ref_1_lvar1 = 1;
    }

  return *ref_1_ref1;

/* { dg-missed {'map\(force_tofrom:ref_1_lvar1 \[len: [0-9]+\]\)' not optimized: ref_1_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static int ref_2 ()
{
  int *ref_2_ref1;
  int ref_2_lvar1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      ref_2_lvar1 = 1;
    }

  ref_2_ref1 = &ref_2_lvar1;
  return *ref_2_ref1;

/* { dg-missed {'map\(force_tofrom:ref_2_lvar1 \[len: [0-9]+\]\)' not optimized: ref_2_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static void ref_3 ()
{
  int ref_3_lvar1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  // FIXME: could be optimized
    {
      int *ref_3_ref1 = &ref_3_lvar1;
      ref_3_lvar1 = 1;
    }

/* { dg-missed {'map\(force_tofrom:ref_3_lvar1 \[len: [0-9]+\]\)' not optimized: ref_3_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static void ref_4 ()
{
  int ref_4_lvar1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  // FIXME: could be optmized
    {
      int *ref_4_ref1 = &ref_4_lvar1;
      *ref_4_ref1 = 1;
    }

/* { dg-missed {'map\(force_tofrom:ref_4_lvar1 \[len: [0-9]+\]\)' not optimized: ref_4_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static void conditional_1 (int conditional_1_pvar1)
{
  int conditional_1_lvar1 = 1;

  if (conditional_1_pvar1)
    {
      // TODO: should be opimizable, but isn't due to later usage in the
      // linear scan.
#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
	{
	  int dummy = conditional_1_lvar1;
	}
    }
  else
    {
      int dummy = conditional_1_lvar1; /* { dg-line l_use[incr c_use] } */
    }

/* { dg-missed {'map\(force_tofrom:conditional_1_lvar1 \[len: [0-9]+\]\)' not optimized: conditional_1_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use } */
}

static void conditional_2 (int conditional_2_pvar1)
{
  int conditional_2_lvar1 = 1;

  if (conditional_2_pvar1)
    {
      int dummy = conditional_2_lvar1;
    }
  else
    {
#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
	{
	  int dummy = conditional_2_lvar1;
	}
    }

/* { dg-optimized {'map\(force_tofrom:conditional_2_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:conditional_2_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute } */
}
