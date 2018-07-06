! { dg-do run }
! { dg-options "-std=gnu" }
! Tests PR32136, which went away!
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
real(kind(0d0)), parameter :: r(1) = &
    transfer(transfer(sqrt(2d0), (/ .true. /) ), (/ 0d0 /), 1)
    if (r(1) .ne. sqrt(2d0)) STOP 1
end

