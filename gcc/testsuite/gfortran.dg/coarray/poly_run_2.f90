! { dg-do run }
!
! Test for polymorphic coarrays
!
type t
end type t
class(t), allocatable :: A[:,:]
allocate (A[1:4,-5:*])
if (allocated(A)) stop
if (any (lcobound(A) /= [1, -5])) STOP 1
if (num_images() == 1) then
  if (any (ucobound(A) /= [4, -5])) STOP 2
else
  if (ucobound(A,dim=1) /= 4) STOP 3
end if
if (allocated(A)) i = 5
call s(A)
call st(A)
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
subroutine st(x)
  class(t) :: x[:,:]
  if (any (lcobound(x) /= [1, -5])) STOP 7
  if (num_images() == 1) then
    if (any (ucobound(x) /= [4, -5])) STOP 8
  else
    if (ucobound(x,dim=1) /= 4) STOP 9
  end if
end subroutine st
end

