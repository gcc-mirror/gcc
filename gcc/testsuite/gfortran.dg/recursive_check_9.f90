! { dg-do run }
! { dg-options "-fcheck=recursion" }
! { dg-shouldfail "Recursion check" }
!
! { dg-output "Fortran runtime error: Recursive call to nonrecursive procedure 'f'" }
!
! PR fortran/39577
!
! Invalid - recursion
program test
 call f(.false.)
 call f(.true.)
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
