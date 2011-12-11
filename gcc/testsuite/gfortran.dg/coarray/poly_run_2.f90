! { dg-do run }
!
! Test for polymorphic coarrays
!
type t
end type t
class(t), allocatable :: A[:,:]
allocate (A[1:4,-5:*])
if (allocated(A)) stop
if (any (lcobound(A) /= [1, -5])) call abort ()
if (num_images() == 1) then
  if (any (ucobound(A) /= [4, -5])) call abort ()
! FIXME: Tree walk issue
!else
!  if (ucobound(A,dim=1) /= 4) call abort ()
end if
if (allocated(A)) i = 5
call s(A)
call st(A)
contains
subroutine s(x)
  class(t) :: x[4,2:*]
  if (any (lcobound(x) /= [1, 2])) call abort ()
  if (num_images() == 1) then
    if (any (ucobound(x) /= [4, 2])) call abort ()
  else
    if (ucobound(x,dim=1) /= 4) call abort ()
  end if
end subroutine s
subroutine st(x)
  class(t) :: x[:,:]
  if (any (lcobound(x) /= [1, -5])) call abort ()
  if (num_images() == 1) then
    if (any (ucobound(x) /= [4, -5])) call abort ()
  else
    if (ucobound(x,dim=1) /= 4) call abort ()
  end if
end subroutine st
end

