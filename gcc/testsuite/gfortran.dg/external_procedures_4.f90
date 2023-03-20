! { dg-do run }
!
! Test the fix for PR87127 in which the references to exfunc cause
! the error "exfunc at (1) is not a function".
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
function exfunc(i)
  implicit none
  integer :: exfunc,i
  exfunc = 2*i
end function

! contents of test.f90
program test
  implicit none
  integer :: exfunc,i
  integer,parameter :: array(2)=[6,7]
  associate(i=>array(2))            ! Original bug
    if (exfunc(i) .ne. 2*i) stop 1
  end associate
  i = 99
  call foo
contains
  subroutine foo()                  ! Comment #3
    if (exfunc(i) .ne. 2*i) stop 2
  end subroutine foo
end program
