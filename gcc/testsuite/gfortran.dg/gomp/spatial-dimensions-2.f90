! { dg-do compile }
! { dg-additional-options "-fdump-tree-omplower -fdump-tree-ompexp" }

! This testcase is ambigous in Fortran
! cf. OpenMP Spec Issues 4998
! Unresolved when this testcase was written

! FIXME: Check what's the outcome of the discussion

! NOTE: GCC currently implements this as spatial dimension
! and not as lower bound ...

subroutine sub
  integer, parameter :: dims(2) = [11,22]
!...
  !$omp teams  num_teams( dims(1) : 256 )
  !$omp end teams
end

! ... hence: 'num_teams(dims():256)' and not 'num_teams(11:256)'
! regarded as idenical to num_teams(256)  (undimensional case)

! { dg-final { scan-tree-dump "#pragma omp teams num_teams\\(dims\\(\\):256\\)" "omplower" } }
! { dg-final { scan-tree-dump "__builtin_GOMP_teams_reg \\(sub_._omp_fn.0, 0B, 256, 0, 0\\);" "ompexp" } }
