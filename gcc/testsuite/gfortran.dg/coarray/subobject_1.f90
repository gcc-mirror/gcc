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
  if (b%a%i /= 7) call abort
  if (any (lcobound(b%a) /= (/ lb /))) call abort
  if (ucobound(b%a, dim=1) /= num_images() + lb - 1) call abort
  if (any (lcobound(b%a%i) /= (/ lb /))) call abort
  if (ucobound(b%a%i, dim=1) /= num_images() + lb - 1) call abort
  allocate(c%a(la)[lc:*])
  c%a%i = init
  if (any(c%a%i /= init)) call abort
  if (any (lcobound(c%a) /= (/ lc /))) call abort
  if (ucobound(c%a, dim=1) /= num_images() + lc - 1) call abort
  if (any (lcobound(c%a%i) /= (/ lc /))) call abort
  if (ucobound(c%a%i, dim=1) /= num_images() + lc - 1) call abort
  if (c%a(2)%i /= init(2)) call abort
  if (any (lcobound(c%a(2)) /= (/ lc /))) call abort
  if (ucobound(c%a(2), dim=1) /= num_images() + lc - 1) call abort
  if (any (lcobound(c%a(2)%i) /= (/ lc /))) call abort
  if (ucobound(c%a(2)%i, dim=1) /= num_images() + lc - 1) call abort
  deallocate(b%a, c%a)
end
