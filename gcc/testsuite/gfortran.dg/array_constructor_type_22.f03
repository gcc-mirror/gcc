! { dg-do compile }
! PR Fortran/83548
program foo

   implicit none

   logical, parameter :: t = .true., f = .false.
   logical, parameter :: a1(2) = [t, f]
   logical(kind=1), parameter :: a2(2) = [logical(kind=1) :: t,  f]
   logical(kind=4), parameter :: a3(2) = [logical(kind=4) :: t,  f]
   logical(kind=1), parameter :: a4(2) = [logical(t, 1), logical(f, 1)]
   logical(kind=4), parameter :: a5(2) = [logical(t, 4), logical(f, 4)]
   logical(kind=1) b(2)
   logical(kind=4) c(2)

   real, parameter :: x = 1, y = 2
   real, parameter :: r1(2) = [x, y]
   real(kind=4), parameter :: r2(2) = [real(kind=4) :: x,  y]
   real(kind=8), parameter :: r3(2) = [real(kind=8) :: x,  y]
   real(kind=4), parameter :: r4(2) = [real(x, 4), real(y, 4)]
   real(kind=8), parameter :: r5(2) = [real(x, 8), real(y, 8)]
   real(kind=4) p(2)
   real(kind=8) q(2)

   p = [real(kind=4) :: x,  y]
   q = [real(kind=8) :: x,  y]
   if (any(p .ne. r2)) STOP 1
   if (any(q .ne. r3)) STOP 2
end program foo
