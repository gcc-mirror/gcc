/* Not prefetching when the step is loop variant.  */
/* { dg-do compile } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O3 -msse2 -fprefetch-loop-arrays -fdump-tree-aprefetch-details --param min-insn-to-prefetch-ratio=3 --param simultaneous-prefetches=10 -fdump-tree-aprefetch-details" } */

double data[16384];
void donot_prefetch_when_non_constant_step_is_variant(int step, int n)
{ 
     int a;
     int b;
     for (a = 1; a < step; a++,step*=2) {
        for (b = 0; b < n; b += 2 * step) {

          int i = 2*(b + a);
          int j = 2*(b + a + step);


          data[j]   = data[i];
          data[j+1] = data[i+1];
        }
     } 
}

/* { dg-final { scan-tree-dump "Not prefetching" "aprefetch" } } */
/* { dg-final { scan-tree-dump "loop variant step" "aprefetch" } } */

/* { dg-final { cleanup-tree-dump "aprefetch" } } */

