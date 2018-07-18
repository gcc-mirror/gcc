! { dg-do run }
! This tests a few reshape PRs.
program resh
  implicit none
  real, dimension (2,3) :: a,c
  real, dimension (12) :: b
  type foo
    real :: r
  end type foo
  type(foo), dimension (2,3) :: ar
  type(foo), dimension (12) :: br

  character (len=80) line1, line2, line3
  integer :: i

  ! PR 21108:  This used to give undefined results.
  b = (/(i,i=1,12)/)
  a = reshape(b(1:12:2),shape(a),order=(/2,1/))
  c = reshape(b(1:12:2),shape(a),order=(/2,1/))
  if (any (a /= c)) STOP 1

  ! Test generic reshape
  br%r = b
  ar = reshape(br(1:12:2),shape(a),order=(/2,1/))
  if (any (ar%r /= a)) STOP 2

  ! Test callee-allocated memory with a write statement
  write (line1,'(6F8.3)') reshape(b(1:12:2),shape(a),order=(/2,1/))
  write (line2,'(6F8.3)') a
  if (line1 /= line2 ) STOP 3
  write (line3,'(6F8.3)') reshape(br(1:12:2),shape(ar),order=(/2,1/))
  if (line1 /= line3 ) STOP 4
end
