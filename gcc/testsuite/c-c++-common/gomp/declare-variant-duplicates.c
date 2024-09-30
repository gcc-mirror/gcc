/* Check that errors are detected if duplicates appear in name-list
   properties.  */

extern int f1 (int);
extern int f2 (int);
extern int f3 (int);
extern int f4 (int);

#pragma omp declare variant (f1) match (device={kind(cpu,gpu,"cpu")})  /* { dg-error "trait-property .cpu. specified more than once" } */
#pragma omp declare variant (f2) match (device={isa(sse4,"avx",avx)})  /* { dg-error "trait-property .avx. specified more than once" } */
#pragma omp declare variant (f3) match (device={arch(x86_64,"i386",aarch64,"x86_64")})  /* { dg-error "trait-property .x86_64. specified more than once" } */
#pragma omp declare variant (f4) match (implementation={vendor(llvm,gnu,"arm",gnu)})  /* { dg-error "trait-property .gnu. specified more than once" } */
int f (int);
