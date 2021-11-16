/* Test 'gcc/omp-data-optimize.c'.  */

/* { dg-additional-options "-std=c++11" } */
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
   "WARNING: dg-line var l_use defined, but not used".  */

static int closure_1 (int closure_1_pvar1)
{
  int closure_1_lvar1 = 1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      closure_1_lvar1 = closure_1_pvar1;
    }

  auto lambda = [closure_1_lvar1]() {return closure_1_lvar1;}; /* { dg-line l_use[incr c_use] } */
  return lambda();

/* { dg-optimized {'map\(force_tofrom:closure_1_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_1_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:closure_1_lvar1 \[len: [0-9]\]\)' not optimized: closure_1_lvar1 used...} "" { target *-*-* } l_compute$c_compute } */
/* { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use } */
}

static int closure_2 (int closure_2_pvar1)
{
  int closure_2_lvar1 = 1;

  auto lambda = [closure_2_lvar1]() {return closure_2_lvar1;};

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      closure_2_lvar1 = closure_2_pvar1;
    }

  return lambda();

/* { dg-optimized {'map\(force_tofrom:closure_2_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_2_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-optimized {'map\(force_tofrom:closure_2_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_2_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:closure_2_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(closure_2_lvar1\)'} "" { target *-*-* } l_compute$c_compute }  */
}

static int closure_3 (int closure_3_pvar1)
{
  int closure_3_lvar1 = 1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      closure_3_lvar1 = closure_3_pvar1;
    }

  auto lambda = [&]() {return closure_3_lvar1;};

  return lambda();

/* { dg-optimized {'map\(force_tofrom:closure_3_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_3_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {map\(force_tofrom:closure_3_lvar1 \[len: [0-9]+\]\)' not optimized: closure_3_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static int closure_4 (int closure_4_pvar1)
{
  int closure_4_lvar1 = 1;

  auto lambda = [&]() {return closure_4_lvar1;};

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      closure_4_lvar1 = closure_4_pvar1;
    }

  return lambda();

/* { dg-optimized {'map\(force_tofrom:closure_4_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_4_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {map\(force_tofrom:closure_4_lvar1 \[len: [0-9]+\]\)' not optimized: closure_4_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }  */
}

static int closure_5 (int closure_5_pvar1)
{
  int closure_5_lvar1 = 1;

  auto lambda = [=]() {return closure_5_lvar1;};

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      closure_5_lvar1 = closure_5_pvar1;
    }

  return lambda();

/* { dg-optimized {'map\(force_tofrom:closure_5_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_5_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-optimized {'map\(force_tofrom:closure_5_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_5_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
   { dg-optimized {'map\(to:closure_5_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(closure_5_lvar1\)'} "" { target *-*-* } l_compute$c_compute }  */
}

static int closure_6 (int closure_6_pvar1)
{
  int closure_6_lvar1 = 1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      closure_6_lvar1 = closure_6_pvar1;
    }

  auto lambda = [=]() {return closure_6_lvar1;}; /* { dg-line l_use[incr c_use] } */

  return lambda();

/* { dg-optimized {'map\(force_tofrom:closure_6_pvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:closure_6_pvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }  */
/* { dg-missed {'map\(force_tofrom:closure_6_lvar1 \[len: [0-9]+\]\)' not optimized: closure_6_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use } */
}

static int try_1 ()
{
  int try_1_lvar1, try_1_lvar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      try_1_lvar1 = 1;
    }

  try {
    try_1_lvar2 = try_1_lvar1; /* { dg-line l_use[incr c_use] } */
  } catch (...) {}

  return try_1_lvar2;

/* { dg-missed {'map\(force_tofrom:try_1_lvar1 \[len: [0-9]+\]\)' not optimized: try_1_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use } */
}

static int try_2 ()
{
  int try_2_lvar1, try_2_lvar2;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    {
      /* { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 }  */
      try_2_lvar1 = 1;
    }

  try {
    try_2_lvar2 = 1;
  } catch (...) {
    try_2_lvar2 = try_2_lvar1; /* { dg-line l_use[incr c_use] } */
  }

  return try_2_lvar2;

/* { dg-missed {'map\(force_tofrom:try_2_lvar1 \[len: [0-9]+\]\)' not optimized: try_2_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
   { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use } */
}
