! { dg-do run }
!
! PR fortran/51972
! Also tests fixes for PR52102
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

! Test allocate with array source - PR52102
  allocate (two%a(2), source = [t(4), t(6)])

  if (allocated (one%a)) call abort ()

  one = two
  if (.not.allocated (one%a)) call abort ()

  if ((one%a(1)%x /= 4)) call abort ()
  if ((one%a(2)%x /= 6)) call abort ()

  deallocate (two%a)
  one = two

  if (allocated (one%a)) call abort ()

! Test allocate with no source followed by assignments.
  allocate (two%a(2))
  two%a(1)%x = 5
  two%a(2)%x = 7

  if (allocated (one%a)) call abort ()

  one = two
  if (.not.allocated (one%a)) call abort ()

  if ((one%a(1)%x /= 5)) call abort ()
  if ((one%a(2)%x /= 7)) call abort ()

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

  if (allocated (one%a)) call abort ()
  if (allocated (two%a)) call abort ()

  allocate (two%a(2))

  if (allocated (two%a(1)%x)) call abort ()
  if (allocated (two%a(2)%x)) call abort ()
  allocate (two%a(1)%x(3), source=[1,2,3])
  allocate (two%a(2)%x(5), source=[5,6,7,8,9])
  one = two
  if (.not. allocated (one%a)) call abort ()
  if (.not. allocated (one%a(1)%x)) call abort ()
  if (.not. allocated (one%a(2)%x)) call abort ()

  if (size(one%a) /= 2) call abort()
  if (size(one%a(1)%x) /= 3) call abort()
  if (size(one%a(2)%x) /= 5) call abort()
  if (any (one%a(1)%x /= [1,2,3])) call abort ()
  if (any (one%a(2)%x /= [5,6,7,8,9])) call abort ()

  deallocate (two%a(1)%x)
  one = two
  if (.not. allocated (one%a)) call abort ()
  if (allocated (one%a(1)%x)) call abort ()
  if (.not. allocated (one%a(2)%x)) call abort ()

  if (size(one%a) /= 2) call abort()
  if (size(one%a(2)%x) /= 5) call abort()
  if (any (one%a(2)%x /= [5,6,7,8,9])) call abort ()

  deallocate (two%a)
  one = two
  if (allocated (one%a)) call abort ()
  if (allocated (two%a)) call abort ()
end subroutine test4


call test1 ()
call test2 ()
call test3 ()
call test4 ()
end

