! Testcase for bound check after issued error
! See PR 94192
! { dg-do compile }
program bound_for_illegal
  
contains

  subroutine bnds(a)  ! { dg-error "must have a deferred shape or assumed rank" }
    integer, pointer, intent(in) :: a(1:2)
    print *,lbound(a)
  end subroutine bnds

end program bound_for_illegal
