! { dg-do run }
! PR fortran/113911
!
! Test that deferred length is not lost

module m
  integer, parameter        :: n = 100, l = 10
  character(l)              :: a = 'a234567890', b(n) = 'bcdefghijk'
  character(:), allocatable :: c1, c2(:)
end

program p
  use m, only : l, n, a, b, x => c1, y => c2
  implicit none
  character(:), allocatable :: d, e(:)
  allocate (d, source=a)
  allocate (e, source=b)
  if (len (d) /= l .or. len (e) /= l .or. size (e) /= n) stop 12
  call plain_deferred (d, e)
  call optional_deferred (d, e)
  call optional_deferred_ar (d, e)
  if (len (d) /= l .or. len (e) /= l .or. size (e) /= n) stop 13
  deallocate (d, e)
  call alloc (d, e)
  if (len (d) /= l .or. len (e) /= l .or. size (e) /= n) stop 14
  deallocate (d, e)
  call alloc_host_assoc ()
  if (len (d) /= l .or. len (e) /= l .or. size (e) /= n) stop 15
  deallocate (d, e)
  call alloc_use_assoc ()
  if (len (x) /= l .or. len (y) /= l .or. size (y) /= n) stop 16
  call indirect (x, y)
  if (len (x) /= l .or. len (y) /= l .or. size (y) /= n) stop 17
  deallocate (x, y)
contains
  subroutine plain_deferred (c1, c2)
    character(:), allocatable :: c1, c2(:)
    if (.not. allocated (c1) .or. .not. allocated (c2)) stop 1
    if (len (c1) /= l) stop 2
    if (len (c2) /= l) stop 3
    if (c1(1:3)    /= "a23") stop 4
    if (c2(5)(1:3) /= "bcd") stop 5
  end

  subroutine optional_deferred (c1, c2)
    character(:), allocatable, optional :: c1, c2(:)
    if (.not. present   (c1) .or. .not. present   (c2)) stop 6
    if (.not. allocated (c1) .or. .not. allocated (c2)) stop 7
    if (len (c1) /= l) stop 8
    if (len (c2) /= l) stop 9
    if (c1(1:3)    /= "a23") stop 10
    if (c2(5)(1:3) /= "bcd") stop 11
  end

  ! Assumed rank
  subroutine optional_deferred_ar (c1, c2)
    character(:), allocatable, optional :: c1(..)
    character(:), allocatable, optional :: c2(..)
    if (.not. present   (c1) .or. &
        .not. present   (c2)) stop 21
    if (.not. allocated (c1) .or. &
        .not. allocated (c2)) stop 22

    select rank (c1)
    rank (0)
    if (len (c1) /= l)       stop 23
      if (c1(1:3)  /= "a23") stop 24
    rank default
      stop 25
    end select

    select rank (c2)
    rank (1)
      if (len (c2) /= l)       stop 26
      if (c2(5)(1:3) /= "bcd") stop 27
    rank default
      stop 28
    end select
  end

  ! Allocate dummy arguments
  subroutine alloc (c1, c2)
    character(:), allocatable :: c1, c2(:)
    allocate (c1, source=a)
    allocate (c2, source=b)
  end

  ! Allocate host-associated variables
  subroutine alloc_host_assoc ()
    allocate (d, source=a)
    allocate (e, source=b)
  end

  ! Allocate use-associated variables
  subroutine alloc_use_assoc ()
    allocate (x, source=a)
    allocate (y, source=b)
  end

  ! Pass-through deferred-length
  subroutine indirect (c1, c2)
    character(:), allocatable :: c1, c2(:)
    call plain_deferred (c1, c2)
    call optional_deferred (c1, c2)
    call optional_deferred_ar (c1, c2)
  end
end
