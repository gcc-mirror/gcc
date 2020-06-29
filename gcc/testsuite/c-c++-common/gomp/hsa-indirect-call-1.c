/* Instead of ICE, we'd like "HSA does not implement indirect calls".  */

/* Reduced from 'libgomp.c/target-39.c'.  */

/* { dg-require-effective-target offload_hsa } */
/* { dg-additional-options "-Whsa" } to override '{gcc,g++}.dg/gomp/gomp.exp'.  */

typedef void (*fnp) (void);
void f1 (void) { }
fnp f2 (void) { return f1; }
#pragma omp declare target to (f1, f2)

int
main ()
{
  #pragma omp target
  {
    fnp fnp = f2 ();
    fnp (); /* { dg-message "note: support for HSA does not implement indirect calls" } */
  }
  return 0;
}

/* { dg-warning "could not emit HSAIL for the function" "" { target *-*-* } 0 } */
