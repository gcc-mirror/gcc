! { dg-do compile }
! { dg-additional-options "-Wno-deprecated-openmp" }

  integer :: var1
  !$omp declare target link(var1)
  !$omp declare target local(var1) ! { dg-error "OMP DECLARE TARGET variable at \\(1\\) previously mentioned in LINK clause and later in LOCAL clause" }

  integer :: var2
  !$omp declare target local(var2)
  !$omp declare target link(var2) ! { dg-error "OMP DECLARE TARGET variable at \\(1\\) previously mentioned in LOCAL clause and later in LINK clause" }

  integer :: var3
  !$omp declare target enter(var3)
  !$omp declare target local(var3) ! { dg-error "OMP DECLARE TARGET variable at \\(1\\) previously mentioned in TO or ENTER clause and later in LOCAL clause" }

  integer :: var4
  !$omp declare target local(var4)
  !$omp declare target enter(var4) ! { dg-error "OMP DECLARE TARGET variable at \\(1\\) previously mentioned in LOCAL clause and later in ENTER clause" }
  !$omp declare target to(var4) ! { dg-error "OMP DECLARE TARGET variable at \\(1\\) previously mentioned in LOCAL clause and later in TO clause" }

  integer :: var5, var6
  !$omp declare target local(var5) local(var5) ! { dg-error "Variable at \\(1\\) mentioned multiple times in clauses of the same OMP DECLARE TARGET directive" }
  !$omp declare target local(var6) enter(var6) ! { dg-error "Variable at \\(1\\) mentioned multiple times in clauses of the same OMP DECLARE TARGET directive" }

end program
