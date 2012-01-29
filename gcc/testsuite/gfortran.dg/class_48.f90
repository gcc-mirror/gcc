! { dg-do run }
!
! PR fortran/51972
!
! Check whether DT assignment with polymorphic components works.
!

subroutine test1 ()
  type t
    integer :: x
  end type t

  type t2
    class(t), allocatable :: a
  end type t2

  type(t2) :: one, two

  one = two
  if (allocated (one%a)) call abort ()

  allocate (two%a)
  two%a%x = 7890
  one = two
  if (one%a%x /= 7890) call abort ()

  deallocate (two%a)
  one = two
  if (allocated (one%a)) call abort ()
end subroutine test1

subroutine test2 ()
  type t
    integer, allocatable :: x(:)
  end type t

  type t2
    class(t), allocatable :: a
  end type t2

  type(t2) :: one, two

  one = two
  if (allocated (one%a)) call abort ()

  allocate (two%a)
  one = two
  if (.not.allocated (one%a)) call abort ()
  if (allocated (one%a%x)) call abort ()

  allocate (two%a%x(2))
  two%a%x(:) = 7890
  one = two
  if (any (one%a%x /= 7890)) call abort ()

  deallocate (two%a)
  one = two
  if (allocated (one%a)) call abort ()
end subroutine test2


subroutine test3 ()
  type t
    integer :: x
  end type t

  type t2
    class(t), allocatable :: a(:)
  end type t2

  type(t2) :: one, two

  one = two
  if (allocated (one%a)) call abort ()

  allocate (two%a(2), source=[t(4), t(6)])
  one = two
  if (.not.allocated (one%a)) call abort ()
! FIXME: Check value

  deallocate (two%a)
  one = two
  if (allocated (one%a)) call abort ()
end subroutine test3

subroutine test4 ()
  type t
    integer, allocatable :: x(:)
  end type t

  type t2
    class(t), allocatable :: a(:)
  end type t2

  type(t2) :: one, two

  one = two
  if (allocated (one%a)) call abort ()

!  allocate (two%a(2)) ! ICE: SEGFAULT
!  one = two
!  if (.not. allocated (one%a)) call abort ()
end subroutine test4


call test1 ()
call test2 ()
call test3 ()
call test4 ()
end
