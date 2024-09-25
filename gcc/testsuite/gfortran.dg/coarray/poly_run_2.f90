! { dg-do run }
!
! Test for polymorphic coarrays
!
type t
end type t
class(t), allocatable :: A[:,:]
allocate (A[1:4,-5:*])
if (any (lcobound(A) /= [1, -5])) STOP 1
if (num_images() == 1) then
  if (any (ucobound(A) /= [4, -5])) STOP 2
else
  if (ucobound(A,dim=1) /= 4) STOP 3
end if

call s(A)
call s2(A)
call sa(A)
contains
subroutine s(x)
  class(t) :: x[4,2:*]
  if (any (lcobound(x) /= [1, 2])) STOP 4
  if (num_images() == 1) then
    if (any (ucobound(x) /= [4, 2])) STOP 5
  else
    if (ucobound(x,dim=1) /= 4) STOP 6
  end if
end subroutine s
subroutine s2(x)
  ! Check that different cobounds are set correctly.
  class(t) :: x[2:5,7:*]
  if (any (lcobound(x) /= [2, 7])) STOP 7
  if (num_images() == 1) then
    if (any (ucobound(x) /= [5, 7])) STOP 8
  else
    if (ucobound(x,dim=1) /= 5) STOP 9
  end if
end subroutine s2
subroutine sa(x)
  class(t), allocatable :: x[:,:]
  if (any (lcobound(x) /= [1, -5])) STOP 10
  if (num_images() == 1) then
    if (any (ucobound(x) /= [4, -5])) STOP 11
  else
    if (ucobound(x,dim=1) /= 4) STOP 12
  end if
end subroutine sa
end

