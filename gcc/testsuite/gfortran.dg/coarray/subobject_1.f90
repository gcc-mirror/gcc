! { dg-do run }
!
! PR fortran/50420
! Coarray subobjects were not accepted as valid coarrays

  integer  :: i
  integer, parameter :: la = 4, lb = 5, lc = 8
  integer, parameter :: init(la) = -4 + (/ (i, i=1,la) /)
  
  type t
    integer :: i
  end type t
  type t2
    type(t), allocatable :: a[:]
  end type t2
  type t3
    type(t), allocatable :: a(:)[:]
  end type t3

  type(t2) :: b
  type(t3) :: c

  allocate(b%a[lb:*])
  b%a%i = 7
  if (b%a%i /= 7) STOP 1
  if (any (lcobound(b%a) /= (/ lb /))) STOP 2
  if (ucobound(b%a, dim=1) /= num_images() + lb - 1) STOP 3
  if (any (lcobound(b%a%i) /= (/ lb /))) STOP 4
  if (ucobound(b%a%i, dim=1) /= num_images() + lb - 1) STOP 5
  allocate(c%a(la)[lc:*])
  c%a%i = init
  if (any(c%a%i /= init)) STOP 6
  if (any (lcobound(c%a) /= (/ lc /))) STOP 7
  if (ucobound(c%a, dim=1) /= num_images() + lc - 1) STOP 8
  if (any (lcobound(c%a%i) /= (/ lc /))) STOP 9
  if (ucobound(c%a%i, dim=1) /= num_images() + lc - 1) STOP 10
  if (c%a(2)%i /= init(2)) STOP 11
  if (any (lcobound(c%a(2)) /= (/ lc /))) STOP 12
  if (ucobound(c%a(2), dim=1) /= num_images() + lc - 1) STOP 13
  if (any (lcobound(c%a(2)%i) /= (/ lc /))) STOP 14
  if (ucobound(c%a(2)%i, dim=1) /= num_images() + lc - 1) STOP 15
  deallocate(b%a, c%a)
end
