/* OpenACC privatization: 'routine' */

/* { dg-additional-options "-fopt-info-omp-note" } */
/* { dg-additional-options "--param=openacc-privatization=noisy" } for
   testing/documenting aspects of that functionality.  */

/* See also '../../gfortran.dg/goacc/privatization-1-routine_gang.f90'.  */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_routine 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

extern int e;
static int s;
int g;
#pragma acc declare device_resident(e, s, g)

#pragma acc routine gang /* { dg-line l_routine[incr c_routine] } */
void
f (int i, int j, int a)
{
  extern int ex;
  static int st;
#pragma acc declare device_resident(ex /* , st */)
  int x, y;
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
#pragma acc declare device_resident(ext /* , sta */)
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
}
  /* { dg-note {variable 'y' declared in block is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 'y' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'x' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'st' declared in block is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 'st' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'ex' declared in block is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 'ex' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'g' declared in block is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 'g' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 's' declared in block is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 's' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'e' declared in block is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 'e' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'a' declared in block potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'j' declared in block potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'i' declared in block potentially has improper OpenACC privatization level: 'parm_decl'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'll' declared in block potentially has improper OpenACC privatization level: 'label_decl'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'struct struct s_ss' declared in block potentially has improper OpenACC privatization level: 'type_decl'} "TODO" { target c xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 's_ss' declared in block potentially has improper OpenACC privatization level: 'type_decl'} "TODO" { target c++ xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'ss' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'func' declared in block potentially has improper OpenACC privatization level: 'function_decl'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'func2' declared in block potentially has improper OpenACC privatization level: 'function_decl'} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'ext' declared in block isn't candidate for adjusting OpenACC privatization level: external} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'sta' declared in block isn't candidate for adjusting OpenACC privatization level: static} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'xx' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "TODO" { xfail *-*-* } l_routine$c_routine } */
  /* { dg-note {variable 'yy' declared in block is candidate for adjusting OpenACC privatization level} "TODO" { xfail *-*-* } l_routine$c_routine }
     { dg-note {variable 'yy' ought to be adjusted for OpenACC privatization level: 'gang'} "TODO" { xfail *-*-* } l_routine$c_routine } */
