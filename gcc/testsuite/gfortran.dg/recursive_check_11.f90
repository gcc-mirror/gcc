! { dg-do run }
! { dg-options "-fcheck=recursion" }
! { dg-shouldfail "Recursion check" }
!
! { dg-output "Fortran runtime error: Recursive call to nonrecursive procedure 'f'" }
!
! PR fortran/39577
!
! wrong - recursion
program test
 integer :: i
 i = f(.false.)
 print *,i
 i = f(.true.)
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
