/* Check that errors are detected if other trait-properties are given with
   kind(any).  */

extern int f1 (int);
extern int f2 (int);
extern int f3 (int);
extern int f4 (int);
extern int f5 (int);
extern int f6 (int);

#pragma omp declare variant (f1) match (device={kind(any,gpu)})  /* { dg-error "no other trait-property may be specified" } */
#pragma omp declare variant (f2) match (device={kind(cpu,"any")})  /* { dg-error "no other trait-property may be specified" } */
#pragma omp declare variant (f3) match (device={kind("any"),arch(x86_64)})  /* { dg-error "no other trait-property may be specified" } */
#pragma omp declare variant (f4) match (device={arch(x86_64),kind(any)})  /* { dg-error "no other trait-property may be specified" } */
#pragma omp declare variant (f5) match (device={kind(any,"any")})  /* { dg-error "trait-property .any. specified more than once" } */
#pragma omp declare variant (f6) match (device={kind("any",any)})  /* { dg-error "trait-property .any. specified more than once" } */
int f (int);
