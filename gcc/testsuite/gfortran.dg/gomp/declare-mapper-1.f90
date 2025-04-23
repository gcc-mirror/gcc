! { dg-do compile }

! Basic "!$omp declare mapper" parsing tests.

module mymod
type s
  integer :: c
  integer :: d(99)
  integer, dimension(100,100) :: e
end type s

!$omp declare mapper (s :: x) map(tofrom: x%c, x%d)
!$omp declare mapper (withaname : s :: x) map(from: x%d(2:30))
!$omp declare mapper (withaname2 : s :: x) map(from: x%d(5))
!$omp declare mapper (named: s :: x) map(tofrom: x%e(:,3))
!$omp declare mapper (named2: s :: x) map(tofrom: x%e(5,:))

end module mymod

program myprog
use mymod, only: s
type t
  integer :: a
  integer :: b
end type t

type u
  integer :: q
end type u

type deriv
  integer :: arr(100)
  integer :: len
end type deriv

type(t) :: y
type(s) :: z
type(u) :: p
type(deriv) :: d
integer, dimension(100,100) :: i2d

!$omp declare mapper (t :: x) map(tofrom: x%a) map(y%b)
!$omp declare mapper (named: t :: x) map(tofrom: x%a) map(y%b)
!$omp declare mapper (integer :: x) ! { dg-error "\\\!\\\$OMP DECLARE MAPPER with non-derived type" }

!$omp declare mapper (deriv :: x) map(tofrom: x%len) &
!$omp & map(tofrom: x%arr(:))

!$omp target map(tofrom: z%e(:,5))
!$omp end target

!$omp target map(mapper(named), tofrom: y)
!$omp end target

!$omp target
y%a = y%b
!$omp end target

d%len = 10

!$omp target
d%arr(5) = 13
!$omp end target

!$omp target map(tofrom: z)
!$omp end target

!$omp target map(mapper(withaname), from: z) map(tofrom:p%q)
!$omp end target

end program myprog
