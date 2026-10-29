! { dg-do run }
! PR libfortran/127751
!
! ALLOCATE of a coarray reports a nonzero STAT= and ERRMSG= when the shared
! memory pool cannot hold the enlarged table of coarray ids.  Without the fix
! the image error terminates and the other images hang.  Retrying a failed
! ALLOCATE of a component of a coarray must not free its token twice.

module m
  implicit none
  type :: t
    integer(1), allocatable :: c(:)
  end type t
  integer :: nok = 0, st_fail = 0
  character(80) :: msg_fail = ''
contains
  subroutine try (x)
    integer(8), allocatable, intent(inout) :: x[:]

    if (st_fail /= 0) return
    allocate (x[*], stat=st_fail, errmsg=msg_fail)
    if (st_fail == 0) nok = nok + 1
  end subroutine try
  subroutine rel (x)
    integer(8), allocatable, intent(inout) :: x[:]

    if (allocated (x)) deallocate (x)
  end subroutine rel
end module m

program alloc_pool_exhausted_3
  use m
  implicit none
  integer, parameter :: nres = 48
  type(t), allocatable :: f(:)[:]
  integer(8), allocatable, codimension[:] :: x01, x02, x03, x04, x05, x06, x07, x08
  integer(8), allocatable, codimension[:] :: x09, x10, x11, x12, x13, x14, x15, x16
  integer(8), allocatable, codimension[:] :: x17, x18, x19, x20, x21, x22, x23, x24
  integer(8), allocatable, codimension[:] :: x25, x26, x27, x28, x29, x30, x31, x32
  integer(8), allocatable, codimension[:] :: x33, x34, x35, x36, x37, x38, x39, x40
  integer(8), allocatable :: y[:]
  integer :: i, k, n, st, nb

  if (num_images () < 2) stop
  allocate (f(nres + 100)[*])

  ! Keep blocks the size of a small coarray in reserve
  do i = 1, nres
    allocate (f(i)%c(8 * num_images ()))
  end do
  sync all
  ! Use up every block of at least 64 bytes
  n = nres
  do k = 30, 6, -1
    do
      if (n == size (f)) stop 1
      allocate (f(n + 1)%c(2**k), stat=st)
      if (st /= 0) exit
      n = n + 1
    end do
  end do
  sync all
  do i = 1, nres
    deallocate (f(i)%c)
  end do
  sync all

  ! New coarray ids eventually need a larger table than the pool can hold
  call try (x01); call try (x02); call try (x03); call try (x04); call try (x05)
  call try (x06); call try (x07); call try (x08); call try (x09); call try (x10)
  call try (x11); call try (x12); call try (x13); call try (x14); call try (x15)
  call try (x16); call try (x17); call try (x18); call try (x19); call try (x20)
  call try (x21); call try (x22); call try (x23); call try (x24); call try (x25)
  call try (x26); call try (x27); call try (x28); call try (x29); call try (x30)
  call try (x31); call try (x32); call try (x33); call try (x34); call try (x35)
  call try (x36); call try (x37); call try (x38); call try (x39); call try (x40)
  if (st_fail == 0) stop 2
  if (len_trim (msg_fail) == 0) stop 3
  if (nok == 0) stop 4

  ! The table and the freed blocks can be reused
  call rel (x01); call rel (x02); call rel (x03); call rel (x04); call rel (x05)
  call rel (x06); call rel (x07); call rel (x08); call rel (x09); call rel (x10)
  call rel (x11); call rel (x12); call rel (x13); call rel (x14); call rel (x15)
  call rel (x16); call rel (x17); call rel (x18); call rel (x19); call rel (x20)
  call rel (x21); call rel (x22); call rel (x23); call rel (x24); call rel (x25)
  call rel (x26); call rel (x27); call rel (x28); call rel (x29); call rel (x30)
  call rel (x31); call rel (x32); call rel (x33); call rel (x34); call rel (x35)
  call rel (x36); call rel (x37); call rel (x38); call rel (x39); call rel (x40)
  allocate (y[*], stat=st)
  if (st /= 0) stop 5
  y = this_image ()
  sync all
  nb = mod (this_image (), num_images ()) + 1
  if (y[nb] /= nb) stop 6
end program alloc_pool_exhausted_3
