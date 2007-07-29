! { dg-do compile }
!
! PR fortran/32906 - Parameter array ... cannot be automatic or assumed shape
!
! Testcase contributed by Florian Ladstaedter <flad AT gmx DOT at>
!
program test_program
  integer, parameter :: len = 1
  integer, parameter :: arr(max(len,1)) = (/1/)

  character(len=*), dimension (1), parameter             :: specStr = (/'string'/)
  double precision, dimension (size(specStr)), parameter :: specNum = (/99.0d0/)
end
