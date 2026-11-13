! { dg-do run }
! { dg-options "-fdump-tree-original" }
! Numeric and logical pointer arrays in I/O lists are transferred as arrays,
! also when they point to a component of an array of derived type.
module m
  implicit none
contains
  subroutine wr (u, p, q, l)
    integer, intent(in) :: u
    real(8), pointer, intent(in) :: p(:)
    integer, pointer, intent(in) :: q(:)
    logical, pointer, intent(in) :: l(:)
    write (u) p, q, l
  end subroutine wr
  subroutine wr2 (u, p)
    integer, intent(in) :: u
    real(8), pointer, intent(in) :: p(:,:)
    write (u) p
  end subroutine wr2
  subroutine rd (u, p)
    integer, intent(in) :: u
    real(8), pointer, intent(in) :: p(:)
    read (u) p
  end subroutine rd
end module m

program main
  use m
  implicit none
  type :: t
     real(8) :: x
     integer :: tag
     logical :: l
     real(8) :: y
  end type t
  type(t), target :: d(100)
  real(8), pointer :: px(:), p2(:,:)
  integer, pointer :: pt(:)
  logical, pointer :: pl(:)
  real(8), allocatable, target :: a(:,:)
  real(8) :: rx(100), r2(20,30)
  integer :: rt(100), i
  logical :: rl(100)

  do i = 1, 100
     d(i) = t (1.0d0 * i, 1000 + i, mod (i, 3) == 0, -1.0d0 * i)
  end do
  a = reshape ([(1.0d0 * i, i = 1, 2400)], [40, 60])
  px => d%x
  pt => d%tag
  pl => d%l
  p2 => a(1:40:2, 1:60:2)

  open (10, file='pointer_array_io_1.bin', form='unformatted', status='replace')
  call wr (10, px, pt, pl)
  call wr2 (10, p2)
  rewind (10)
  read (10) rx, rt, rl
  read (10) r2
  if (any (rx /= d%x)) stop 1
  if (any (rt /= d%tag)) stop 2
  if (any (rl .neqv. d%l)) stop 3
  if (any (r2 /= a(1:40:2, 1:60:2))) stop 4

  rewind (10)
  d%x = 0
  call rd (10, px)
  close (10, status='delete')
  if (any (d%x /= [(1.0d0 * i, i = 1, 100)])) stop 5
  if (any (d%y /= [(-1.0d0 * i, i = 1, 100)])) stop 6
  if (any (d%tag /= [(1000 + i, i = 1, 100)])) stop 7
end program main
! { dg-final { scan-tree-dump-times "_gfortran_transfer_array_span_write" 4 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_transfer_array_span " 1 "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_transfer_real_write" "original" } }
