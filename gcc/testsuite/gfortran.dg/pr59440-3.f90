! PR fortran/59440
! { dg-do compile }
! { dg-options "-O2 -g" }

subroutine foo (nnml, outv)
  integer, intent(in) :: nnml
  integer, intent(out) :: outv
  integer :: grid
  call bar
  outv = grid
contains
  subroutine bar
    namelist /N/ grid
    read (nnml, nml=N)
  end subroutine bar
end subroutine foo
