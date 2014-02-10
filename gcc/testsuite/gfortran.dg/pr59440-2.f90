! PR fortran/59440
! { dg-do compile }
! { dg-options "-O2 -g" }

subroutine foo (nnml, outv)
  integer, intent(in) :: nnml
  integer, intent(out) :: outv
  integer :: grid
  namelist /N/ grid
  read (nnml, nml=N)
  call bar
contains
  subroutine bar
    outv = grid
  end subroutine bar
end subroutine foo
