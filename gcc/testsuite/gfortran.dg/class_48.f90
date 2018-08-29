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
  if (allocated (one%a)) STOP 1

  allocate (two%a)
  two%a%x = 7890
  one = two
  if (one%a%x /= 7890) STOP 2

  deallocate (two%a)
  one = two
  if (allocated (one%a)) STOP 3
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
  if (allocated (one%a)) STOP 4

  allocate (two%a)
  one = two
  if (.not.allocated (one%a)) STOP 5
  if (allocated (one%a%x)) STOP 6

  allocate (two%a%x(2))
  two%a%x(:) = 7890
  one = two
  if (any (one%a%x /= 7890)) STOP 7

  deallocate (two%a)
  one = two
  if (allocated (one%a)) STOP 8
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

  if (allocated (one%a)) STOP 9

  one = two
  if (.not.allocated (one%a)) STOP 10

  if ((one%a(1)%x /= 4)) STOP 11
  if ((one%a(2)%x /= 6)) STOP 12

  deallocate (two%a)
  one = two

  if (allocated (one%a)) STOP 13

! Test allocate with no source followed by assignments.
  allocate (two%a(2))
  two%a(1)%x = 5
  two%a(2)%x = 7

  if (allocated (one%a)) STOP 14

  one = two
  if (.not.allocated (one%a)) STOP 15

  if ((one%a(1)%x /= 5)) STOP 16
  if ((one%a(2)%x /= 7)) STOP 17

  deallocate (two%a)
  one = two
  if (allocated (one%a)) STOP 18
end subroutine test3

subroutine test4 ()
  type t
    integer, allocatable :: x(:)
  end type t

  type t2
    class(t), allocatable :: a(:)
  end type t2

  type(t2) :: one, two

  if (allocated (one%a)) STOP 19
  if (allocated (two%a)) STOP 20

  allocate (two%a(2))

  if (allocated (two%a(1)%x)) STOP 21
  if (allocated (two%a(2)%x)) STOP 22
  allocate (two%a(1)%x(3), source=[1,2,3])
  allocate (two%a(2)%x(5), source=[5,6,7,8,9])
  one = two
  if (.not. allocated (one%a)) STOP 23
  if (.not. allocated (one%a(1)%x)) STOP 24
  if (.not. allocated (one%a(2)%x)) STOP 25

  if (size(one%a) /= 2) STOP 26
  if (size(one%a(1)%x) /= 3) STOP 27
  if (size(one%a(2)%x) /= 5) STOP 28
  if (any (one%a(1)%x /= [1,2,3])) STOP 29
  if (any (one%a(2)%x /= [5,6,7,8,9])) STOP 30

  deallocate (two%a(1)%x)
  one = two
  if (.not. allocated (one%a)) STOP 31
  if (allocated (one%a(1)%x)) STOP 32
  if (.not. allocated (one%a(2)%x)) STOP 33

  if (size(one%a) /= 2) STOP 34
  if (size(one%a(2)%x) /= 5) STOP 35
  if (any (one%a(2)%x /= [5,6,7,8,9])) STOP 36

  deallocate (two%a)
  one = two
  if (allocated (one%a)) STOP 37
  if (allocated (two%a)) STOP 38
end subroutine test4


call test1 ()
call test2 ()
call test3 ()
call test4 ()
end

