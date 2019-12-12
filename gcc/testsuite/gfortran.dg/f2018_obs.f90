! { dg-do compile }
! { dg-options "-std=f2018" }
!
! PR 85839: [F2018] warn for obsolescent features
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

block data                           ! { dg-warning "obsolescent feature" }
  common /a/ y(3)                    ! { dg-warning "obsolescent feature" }
  data y /3*1./
end

program f2018_obs

  implicit none
  integer :: a(10),b(10),j(8),i
  real :: x(3)
  common /c/ x                       ! { dg-warning "obsolescent feature" }

  equivalence (a(10),b(1))           ! { dg-warning "obsolescent feature" }

  do 99 i=1,10                       ! { dg-warning "obsolescent feature" }
99 continue

  j = (/ 0, 1, 2, 3, 4, 0, 6, 7  /)
  forall (i=1:8, j(i) /= 0)          ! { dg-warning "obsolescent feature" }
    j(i) = 0
  end forall
end
