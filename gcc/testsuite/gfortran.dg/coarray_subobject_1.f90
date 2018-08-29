! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/50420
! Coarray subobjects were not accepted as valid coarrays
! They should still be rejected if one of the component reference is allocatable
! or pointer

type t
  integer :: i
end type t
type t2
  type(t), allocatable :: a
  type(t), pointer     :: c
end type t2
type(t2) :: b[5:*]
allocate(b%a)
allocate(b%c)
b%a%i = 7
b%c%i = 13
if (b%a%i /= 7) STOP 1
if (any (lcobound(b%a) /= (/ 5 /))) STOP 2! { dg-error "Expected coarray variable" }
if (ucobound(b%a, dim=1) /= this_image() + 4) STOP 3! { dg-error "Expected coarray variable" }
if (any (lcobound(b%a%i) /= (/ 5 /))) STOP 4! { dg-error "Expected coarray variable" }
if (ucobound(b%a%i, dim=1) /= this_image() + 4) STOP 5! { dg-error "Expected coarray variable" }
if (b%c%i /= 13) STOP 6
if (any (lcobound(b%c) /= (/ 5 /))) STOP 7! { dg-error "Expected coarray variable" }
if (ucobound(b%c, dim=1) /= this_image() + 4) STOP 8! { dg-error "Expected coarray variable" }
if (any (lcobound(b%c%i) /= (/ 5 /))) STOP 9! { dg-error "Expected coarray variable" }
if (ucobound(b%c%i, dim=1) /= this_image() + 4) STOP 10! { dg-error "Expected coarray variable" }
end
