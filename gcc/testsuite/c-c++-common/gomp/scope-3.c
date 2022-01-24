/* PR middle-end/102415 */

extern
#ifdef __cplusplus
"C"
#endif
void abort ();

void
foo (void)
{
  #pragma omp scope nowait
  abort ();
}

void
bar (void)
{
  #pragma omp scope
  abort ();
}
