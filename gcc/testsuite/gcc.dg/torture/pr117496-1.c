/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */


/* PR tree-optimization/117496 */
/* This would go into an infinite loop into VN while recording
   the predicates for the `tracks == 0 && wm == 0` GIMPLE_COND.
   As wm_N and tracks_N would valueize back to `tracks | wm`.  */

int main_argc, gargs_preemp, gargs_nopreemp;
static void gargs();
void main_argv() {
  int tracks = 0;
  gargs(main_argc, main_argv, &tracks);
}
void gargs(int, char *, int *tracksp) {
  int tracks = *tracksp, wm;
  for (;;) {
    if (tracks == 0)
      wm |= 4;
    if (gargs_nopreemp)
      gargs_preemp = 0;
    if (tracks == 0 && wm == 0)
      tracks++;
  }
}
