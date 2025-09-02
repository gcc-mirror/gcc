! { dg-do run }
!
! Check PDT type extension and simple OOP.
!
module vars
  integer :: d_dim = 4
  integer :: mat_dim = 256
  integer, parameter :: ftype = kind(0.0)
end module

  use vars
  implicit none
  integer :: i
  type :: mytype (a,b)
    integer, kind :: a = kind(0.0e0)
    integer, LEN :: b = 4
    integer :: i
    real(kind = a) :: d(b, b)
  end type

  type, extends(mytype) :: thytype(h)
    integer, kind :: h
    integer(kind = h) :: j
  end type

  type x (q, r, s)
    integer, kind :: q
    integer, kind :: r
    integer, LEN :: s
    integer(kind = q) :: idx_mat(2,2)  ! check these do not get treated as pdt_arrays.
    type (mytype (b=s)) :: mat1
    type (mytype (b=s*2)) :: mat2
  end type x

  real, allocatable :: matrix (:,:)
  type(thytype(ftype, 4, 4)) :: w
  type(x(ftype,ftype,256)) :: q
  class(mytype(ftype, :)), allocatable :: cz

  w%d = reshape ([(real(i), i = 1, d_dim*d_dim)],[d_dim,d_dim])

! Make sure that the type extension is ordering the parameters correctly.
  if (w%a .ne. ftype) STOP 1
  if (w%b .ne. 4) STOP 2
  if (w%h .ne. 4) STOP 3
  if (size (w%d) .ne. 16) STOP 4
  if (int (w%d(2,4)) .ne. 14) STOP 5
  if (kind (w%j) .ne. w%h) STOP 6

! As a side issue, ensure PDT components are OK
  if (q%mat1%b .ne. q%s) STOP 7
  if (q%mat2%b .ne. q%s*2) STOP 8
  if (size (q%mat1%d) .ne. mat_dim**2) STOP 9
  if (size (q%mat2%d) .ne. 4*mat_dim**2) STOP 10

! Now check some basic OOP with PDTs
  matrix = w%d

! TODO - for some reason, using w%d directly in the source causes a seg fault.
  allocate (cz, source = mytype(ftype, d_dim)( 0, matrix))
  select type (cz)
    type is (mytype(ftype, *))
      if (int (sum (cz%d)) .ne. 136) STOP 11
    type is (thytype(ftype, *, ftype))
      STOP 12
  end select
  deallocate (cz)

  allocate (thytype(ftype, d_dim*2, ftype) :: cz)
  cz%d = reshape ([(i*10, i = 1, cz%b**2)], [cz%b,cz%b])
  select type (cz)
    type is (mytype(ftype, *))
      STOP 13
    type is (thytype(ftype, *, ftype))
      if (int (sum (cz%d)) .ne. 20800) STOP 14
  end select

  deallocate (cz)
end
