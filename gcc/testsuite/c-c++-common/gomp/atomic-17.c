int i, v;
float f;

void
foo ()
{
  #pragma omp atomic release, hint (0), update
  i = i + 1;
  #pragma omp atomic hint(0)seq_cst
  i = i + 1;
  #pragma omp atomic relaxed,update,hint (0)
  i = i + 1;
  #pragma omp atomic release
  i = i + 1;
  #pragma omp atomic relaxed
  i = i + 1;
  #pragma omp atomic acq_rel capture
  v = i = i + 1;
  #pragma omp atomic capture,acq_rel , hint (1)
  v = i = i + 1;
  #pragma omp atomic hint(0),acquire capture
  v = i = i + 1;
  #pragma omp atomic read acquire
  v = i;
  #pragma omp atomic release,write
  i = v;
  #pragma omp atomic hint(1),update,release
  f = f + 2.0;
}
