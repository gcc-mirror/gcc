! { dg-do compile }
!
! PR 55352: [4.7/4.8 Regression] Erroneous gfortran warning of unused module variable when variable is only used in namelist
!
! Contributed by <AstroFloyd@gmail.com>

module data
  implicit none
  integer :: a
end module data

program test
  use data, only: a
  implicit none
  a = 1
  call write_data()
end program test

subroutine write_data()
  use data, only: a
  implicit none
  namelist /write_data_list/ a
  open(unit=10,form='formatted',status='replace',action='write',file='test.dat')
  write(10, nml=write_data_list)
  close(10)
end subroutine write_data
