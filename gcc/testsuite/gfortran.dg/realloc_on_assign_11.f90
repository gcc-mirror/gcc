! { dg-do run }
! PR52012 - tests of automatic reallocation on assignment for variable = array_intrinsic
!
! Contributed by Tobias Burnus and Dominique Dhumieres
!
  integer, allocatable :: a(:), b(:), e(:,:)
  integer :: c(1:5,1:5), d(1:5,1:5)
  allocate(b(3))
  b = [1,2,3]

! Shape conforms so bounds follow allocation.
  allocate (a(7:9))
  a = reshape( b, shape=[size(b)])
  if (any ([lbound(a), ubound(a), size(a), shape (a)] .ne. [7,9,3,3])) call abort

  deallocate (a)
! 'a' not allocated so lbound defaults to 1.
  a = reshape( b, shape=[size(b)])
  if (any ([lbound(a), ubound(a), size(a), shape (a)] .ne. [1,3,3,3])) call abort

  deallocate (a)
! Shape conforms so bounds follow allocation.
  allocate (a(0:0))
  a(0) = 1
  if (any ([lbound(a), ubound(a), size(a), shape (a)] .ne. [0,0,1,1])) call abort

! 'a' not allocated so lbound defaults to 1.
  e = matmul (c(2:5,:), d(:, 3:4))
  if (any ([lbound(e), ubound(e), size(e), shape (e)] .ne. [1,1,4,2,8,4,2])) call abort
  deallocate (e)

! Shape conforms so bounds follow allocation.
  allocate (e(4:7, 11:12))
  e = matmul (c(2:5,:), d(:, 3:4))
  if (any ([lbound(e), ubound(e), size(e), shape (e)] .ne. [4,11,7,12,8,4,2])) call abort
end
