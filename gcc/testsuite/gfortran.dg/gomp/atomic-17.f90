module m
implicit none
integer i, v
real f
contains

subroutine foo ()
  !$omp atomic release, hint (0), update
  i = i + 1
  !$omp atomic hint(0)seq_cst
  i = i + 1
  !$omp atomic relaxed,update,hint (0)
  i = i + 1
  !$omp atomic release
  i = i + 1
  !$omp atomic relaxed
  i = i + 1
  !$omp atomic acq_rel capture
  i = i + 1; v = i
  !$omp atomic capture,acq_rel , hint (1)
  i = i + 1; v = i
  !$omp atomic hint(0),acquire capture
  i = i + 1; v = i
  !$omp atomic read acquire
  v = i
  !$omp atomic acq_rel read
  v = i
  !$omp atomic release,write
  i = v
  !$omp atomic write,acq_rel
  i = v
  !$omp atomic hint(1),update,release
  f = f + 2.0
  !$omp atomic update ,acquire
  i = i + 1
  !$omp atomic acq_rel update
  i = i + 1
  !$omp atomic acq_rel,hint(0)
  i = i + 1
end
end module
