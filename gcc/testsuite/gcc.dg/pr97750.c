/* PR tree-optimization/97750 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wextra" } */
/* { dg-require-effective-target ptr_eq_long } */

char CopyPlane_src;
long CopyPlane_copy_pitch;
char *CopyFromUswc_src;
int CopyFromUswc_height;
void CopyPlane(char *dst) {
  __builtin_memcpy(dst, &CopyPlane_src, CopyPlane_copy_pitch);
}
void CopyFromUswc(long src_pitch) {
  char *dst;
  for (; CopyFromUswc_height;) {
    unsigned unaligned = (long)CopyFromUswc_src;
    if (unaligned)
      CopyPlane(&dst[unaligned]);  
    CopyFromUswc_src += src_pitch;
  }
}
/* { dg-prune-output "-Wmaybe-uninitialized" } */
