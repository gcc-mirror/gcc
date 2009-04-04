! { dg-do run }
! { dg-options "-fcheck=recursion" }
!
! PR fortran/39577
!
! OK - no recursion
program test
 integer :: i
 i = f(.false.)
 print *,i
 i = f(.false.)
 print *,i
contains
  integer function f(rec) 
    logical :: rec
    if(rec) then
      f = g()
    else
      f = 42
    end if
  end function f
  integer function g()
    g = f(.false.)
  end function g
end program test
