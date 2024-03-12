! { dg-do run }
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
program main
  character(2) :: c

  type :: S
    integer :: n
  end type
  type(S) :: Sobj

  type, extends(S) :: S2
    integer :: m
  end type
  type(S2) :: S2obj

  type :: T
    class(S), allocatable :: x
  end type

  type tContainer
    class(*), allocatable :: x
  end type

  type(T) :: Tobj

  Sobj = S(1)
  Tobj = T(Sobj)

  S2obj = S2(1,2)
  Tobj = T(S2obj)            ! Failed here
  select type (x => Tobj%x)
    type is (S2)
      if ((x%n .ne. 1) .or. (x%m .ne. 2)) stop 1
    class default
      stop 2
  end select

  c = "  "
  call pass_it (T(Sobj))
  if (c .ne. "S ") stop 3
  call pass_it (T(S2obj))    ! and here
  if (c .ne. "S2") stop 4

  call bar

contains

  subroutine pass_it (foo)
    type(T), intent(in) :: foo
    select type (x => foo%x)
      type is (S)
        c = "S "
        if (x%n .ne. 1) stop 5
      type is (S2)
        c = "S2"
        if ((x%n .ne. 1) .or. (x%m .ne. 2)) stop 6
      class default
        stop 7
    end select
  end subroutine

  subroutine check_it (t, errno)
    type(tContainer)  :: t
    integer :: errno
    select type (x => t%x)
      type is (integer)
        if (x .ne. 42) stop errno
      type is (integer(8))
        if (x .ne. 42_8) stop errno
      type is (real(8))
        if (int(x**2) .ne. 2) stop errno
      type is (character(*, kind=1))
        if (x .ne. "end of tests") stop errno
      type is (character(*, kind=4))
        if ((x .ne. 4_"hello!") .and. (x .ne. 4_"goodbye")) stop errno
       class default
        stop errno
    end select
  end subroutine

  subroutine bar
   ! Test from comment #29 extended by Harald Anlauf to check kinds /= default
    integer(8), parameter :: i = 0_8
    integer :: j = 42
    character(7,kind=4) :: chr4 = 4_"goodbye"
    type(tContainer) :: cont

    cont%x = j
    call check_it (cont, 8)

    cont = tContainer(i+42_8)
    call check_it (cont, 9)

    cont = tContainer(sqrt (2.0_8))
    call check_it (cont, 10)

    cont = tContainer(4_"hello!")
    call check_it (cont, 11)

    cont = tContainer(chr4)
    call check_it (cont, 12)

    cont = tContainer("end of tests")
    call check_it (cont, 13)

  end subroutine bar
end program
