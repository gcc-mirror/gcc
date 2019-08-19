! { dg-do compile }
! Fixes PR37787 where the arithmetic overflow was not detected and an ICE ensued.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
! In F2008 and F2018, overflow cannot happen, but a BOZ cannot appear 
! in an array constructor.
!
program bug
  implicit none
   integer(1) :: a(2) = (/ Z'FF', Z'FF' /) ! { dg-error "cannot appear in" }
end program bug
