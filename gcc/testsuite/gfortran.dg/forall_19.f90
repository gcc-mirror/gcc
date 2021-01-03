! { dg-do run }
! PR fortran/98307 - Dependency check fails when using "allocatable"

program forall_deps
  implicit none
  type t
    logical :: valid = .true.
    integer :: s     = 0
    integer, allocatable :: p(:)
  end type
  type(t) :: v(2)
  integer :: i

  allocate (v(1)%p(8))
  allocate (v(2)%p(8))
  v(1)%s    = 8
  v(2)%s    = 6

  v(1)%p(:) = [1, 2, 3, 4, 5, 6, 7, 8]
  v(2)%p(:) = [13, 14, 15, 16, 17, 18, 19, 20]
  forall (i=1:2)
     v(i)%p(1:v(i)%s) = v(3-i)%p(1:v(i)%s)
  end forall
  if (any(v(2)%p(:) /= [1, 2, 3, 4, 5, 6, 19, 20])) stop 1

  v(1)%p(:) = [1, 2, 3, 4, 5, 6, 7, 8]
  v(2)%p(:) = [13, 14, 15, 16, 17, 18, 19, 20]
  forall (i=1:2, v(i)%valid)
     v(i)%p(1:v(i)%s) = v(3-i)%p(1:v(i)%s)
  end forall
  if (any(v(2)%p(:) /= [1, 2, 3, 4, 5, 6, 19, 20])) stop 2
end
