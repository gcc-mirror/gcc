! { dg-do compile }
! PR fortran/59910
!
program main
  implicit none
  type bar
      integer :: limit(1)
  end type
  type (bar) :: testsuite
  data testsuite / bar(reshape(source=[10],shape=[1])) /
end
