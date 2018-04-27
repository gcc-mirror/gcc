! { dg-do run }

! Test that index vector on lhs of caf-expression works correctly.

program pr81773

  integer, parameter :: ndim = 5
  integer :: i
  integer :: vec(ndim) = -1
  integer :: res(ndim)[*] = [ (i, i=1, ndim) ]
  type T
    integer :: padding
    integer :: dest(ndim)
    integer :: src(ndim)
  end type

  type(T) :: dest
  type(T), allocatable :: caf[:]

  vec([ndim, 3, 1]) = res(1:3)[1]
  if (any (vec /= [ 3, -1, 2, -1, 1])) stop 1

  dest = T(42, [ ( -1, i = 1, ndim ) ], [ ( i - 2, i = ndim, 1, -1) ] )
  dest%dest([ 4,3,2 ]) = res(3:5)[1]
  if (any (dest%dest /= [-1, 5, 4, 3, -1])) stop 2

  vec(:) = -1
  allocate(caf[*], source = T(42, [ ( -1, i = 1, ndim ) ], [ ( i - 2, i = ndim, 1, -1) ] ))
  vec([ 5,3,2 ]) = caf[1]%src(2:4)
  if (any (vec /= [ -1, 0, 1, -1, 2])) stop 3
end

