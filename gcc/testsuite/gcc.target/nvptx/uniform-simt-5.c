/* Verify that '-muniform-simt' code may be executed single-threaded.

   { dg-do run }
   { dg-options {-save-temps -O2 -muniform-simt} } */

enum memmodel
{
  MEMMODEL_RELAXED = 0
};

unsigned long long int v64;
unsigned long long int *p64 = &v64;

int
main()
{
  /* Trigger uniform-SIMT processing.  */
  __atomic_fetch_add (p64, v64, MEMMODEL_RELAXED);

  return 0;
}

/* Per 'omp_simt_exit':
     - 'nvptx_warpsync'
       { dg-final { scan-assembler-times {bar\.warp\.sync\t0xffffffff;} 1 { target nvptx_default_ptx_isa_version_at_least_6_0 } } }
     - 'nvptx_uniform_warp_check'
       { dg-final { scan-assembler-times {vote\.all\.pred\t%r_sync, 1;} 1 { target { ! nvptx_default_ptx_isa_version_at_least_6_0 } } } }
*/
