! { dg-do compile }
!
! PR fortran/51055
! PR fortran/45170 comment 14
!
! Contributed by Juha Ruokolainen
! and Hans-Werner Boschmann
!
! gfortran was before checking whether the length
! was a specification expression.
!

program a
  character(len=:), allocatable :: s
  integer :: i=10
  allocate(character(len=i)::s)
end program a
