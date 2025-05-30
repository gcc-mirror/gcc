! { dg-do compile }
! PR fortran/120355 - this was rejected because the typespec from
! the RESULT clause was not picked up.
! Test case jsberg@bnl.gov.

program p
  implicit none
  integer :: i,j
  interface
     function s(x) result(y)
       implicit none
       integer, intent(in) :: x
       integer :: y
     end function s
  end interface
  i = 0
  call t(s,i,j)
contains
  subroutine t(f,x,y)
    implicit none
    integer, intent(in) :: x
    integer, intent(out) :: y
    interface
       function f(x) result(y)
         implicit none
         integer, intent(in) :: x
         integer :: y
       end function f
    end interface
    y = f(x)
  end subroutine t
end program p

function s(x) result(y)
  implicit none
  integer, intent(in) :: x
  integer :: y
  y = 1 - x
end function s
