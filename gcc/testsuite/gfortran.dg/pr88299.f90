! { dg-do compile }
! { dg-options "-std=f2018" }
!
! PR 85839: [F18] COMMON in a legacy module produces bogus warnings
!           in dependent code

module legacy
  integer :: major, n
  common /version/ major  ! { dg-warning "obsolescent feature" }
  public  :: n
  private
end module legacy

module mod1
  use legacy, only: n     ! No warning expected here
end module mod1
