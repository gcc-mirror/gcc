! { dg-do compile }
! Fixes PR37787 where the arithmetic overflow was not detected and an ICE ensued.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
program bug
  implicit none
   integer(1) :: a(2) = (/ Z'FF', Z'FF' /) ! { dg-error "Arithmetic overflow" }
   print*, a
end program bug
