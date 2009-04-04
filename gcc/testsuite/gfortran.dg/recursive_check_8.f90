! { dg-do run }
! { dg-options "-fcheck=recursion" }
!
! PR fortran/39577
!
! OK - no recursion
program test
 call f(.false.)
 call f(.false.)
contains
  subroutine f(rec) 
    logical :: rec
    if(rec) then
      call g()
    end if
    return
  end subroutine f
  subroutine g()
    call f(.false.)
    return
  end subroutine g
end program test
