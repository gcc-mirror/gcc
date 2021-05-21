/* OpenACC privatization: compute construct */

/* { dg-additional-options "-fopt-info-omp-note" } */
/* { dg-additional-options "--param=openacc-privatization=noisy" } for
   testing/documenting aspects of that functionality.  */

/* See also '../../gfortran.dg/goacc/privatization-1-compute.f90'.  */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

extern int e;
static int s;
int g;

void
f (int i, int j, int a)
{
  extern int ex;
  static int st;
  int x, y;
#pragma acc parallel private(i, j, a) private (e, s, g) private(ex, st, x, y) /* { dg-line l_compute[incr c_compute] } */
      {
	__label__ ll;
	/* Nested scopes fun.  */
	{
	  struct s_ss { int i; } ss;
	  {
	    extern int func (int *, int *, int *);
	    /* Don't know how to effect a 'CONST_DECL' here.  (See Fortran example.)  */
	    /* Don't know how to effect a 'RESULT_DECL' here; only saw this for OpenMP 'lastprivate'.  */

	    a = func (&i, &j, &a);
	  }
	  ss.i = a;
	  {
	    extern int func2 (int *, int *, int *, int *, int *, int *, int *);
	    extern int ext;
	    static int sta;
	    a = func2 (&e, &s, &g, &ex, &st, &ext, &sta);
	  }
	}
	x = a;
#pragma acc atomic write
	y = a;
	{
	  int xx, yy;
	  xx = a;
#pragma acc atomic write
	  yy = a;
	}

      ll:
	;
      }
  /* { dg-note {variable 'y' in 'private' clause is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_compute$c_compute }
     { dg-note {variable 'y' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'st' in 'private' clause is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_compute$c_compute }
     { dg-note {variable 'st' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'ex' in 'private' clause is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_compute$c_compute }
     { dg-note {variable 'ex' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'g' in 'private' clause is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_compute$c_compute }
     { dg-note {variable 'g' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 's' in 'private' clause is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_compute$c_compute }
     { dg-note {variable 's' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'e' in 'private' clause is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_compute$c_compute }
     { dg-note {variable 'e' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'a' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'j' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'i' in 'private' clause potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'll' declared in block potentially has improper OpenACC privatization level: 'label_decl'} "TODO" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'struct struct s_ss' declared in block potentially has improper OpenACC privatization level: 'type_decl'} "TODO" { target c } l_compute$c_compute }
     { dg-note {variable 's_ss' declared in block potentially has improper OpenACC privatization level: 'type_decl'} "TODO" { target c++ } l_compute$c_compute } */
  /* { dg-note {variable 'ss' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'func' declared in block potentially has improper OpenACC privatization level: 'function_decl'} "TODO" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'func2' declared in block potentially has improper OpenACC privatization level: 'function_decl'} "TODO" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'ext' declared in block isn't candidate for adjusting OpenACC privatization level: external} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'sta' declared in block isn't candidate for adjusting OpenACC privatization level: static} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'xx' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'yy' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'yy' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_compute$c_compute } */
}
