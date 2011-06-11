! { dg-do run }
!
! PR fortran/49324
!
! Check that with array constructors a deep copy is done
!
implicit none
type t
  integer, allocatable :: A(:)
end type t

type(t) :: x, y
type(t), allocatable :: z(:), z2(:)

allocate (x%A(2))
allocate (y%A(1))
x%A(:) = 11
y%A(:) = 22

allocate (z(2))

z = [ x, y ]
!print *, z(1)%a, z(2)%a, x%A, y%A
if (any (z(1)%a /= 11) .or. z(2)%a(1) /= 22 .or. any (x%A /= 11)  &
    .or. y%A(1) /= 22)  &
  call abort()

x%A(:) = 444
y%A(:) = 555

!print *, z(1)%a, z(2)%a, x%A, y%A
if (any (z(1)%a /= 11) .or. z(2)%a(1) /= 22 .or. any (x%A /= 444)  &
    .or. y%A(1) /= 555)  &
  call abort()

z(:) = [ x, y ]
!print *, z(1)%a, z(2)%a, x%A, y%A
if (any (z(1)%a /= 444) .or. z(2)%a(1) /= 555 .or. any (x%A /= 444)  &
    .or. y%A(1) /= 555)  &
  call abort()
end
