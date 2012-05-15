! { dg-do run }
! { dg-options "-fcheck=recursion" }
!
! PR fortran/39577
!
! OK - no recursion
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
 call f(.false.)
end program test
