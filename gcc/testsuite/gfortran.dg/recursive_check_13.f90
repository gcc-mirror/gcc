! { dg-do run }
! { dg-options "-fcheck=recursion" }
! { dg-shouldfail "Recursion check" }
!
! { dg-output "Fortran runtime error: Recursive call to nonrecursive procedure 'master.0.f'" }
!
! PR fortran/39577
!
! invalid - recursion
module m
  implicit none
contains
  subroutine f(rec) 
    logical :: rec
    if(rec) then
      call h()
    end if
    return
  entry g()
  end subroutine f
  subroutine h()
    call f(.false.)
  end subroutine h
end module m

program test
 use m
 implicit none
 call f(.false.)
 call f(.true.)
end program test
