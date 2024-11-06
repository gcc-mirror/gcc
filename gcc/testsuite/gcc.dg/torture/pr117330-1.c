/* { dg-do compile } */

/* This used to ICE during vectorization
   dealing with the removal of comparisons inside COND_EXPR. */
/* PR tree-optimization/117330 */


enum psi_states {
  PSI_IO_NONE,
  PSI_IO_ONE,
  PSI_IO_TWO,
};
void f(unsigned *times,
       unsigned *times1,
       unsigned state_mask) {
  enum psi_states s;
  for (s = 0; s < 2; s++) {
    if (state_mask & (1 << s))
      times[s] += 1;
    times[s] = times[s] - times1[s];
  }
}
