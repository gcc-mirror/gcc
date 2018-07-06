! { dg-do run }
!
! Make sure that the fix for pr34640 works with class pointers.
!
  type :: mytype
    real :: r
    integer :: i
  end type

  type :: thytype
    real :: r
    integer :: i
    type(mytype) :: der
  end type

  type(thytype), dimension(0:2), target :: tgt
  class(*), dimension(:), pointer :: cptr
  class(mytype), dimension(:), pointer :: cptr1
  integer :: i
  integer(8) :: s1, s2

  tgt = [(thytype(int(i), i, mytype(int(2*i), 2*i)), i= 1,3)]

  cptr => tgt%i
  if (lbound (cptr, 1) .ne. 1)  STOP 1! Not a whole array target!

  s1 = loc(cptr)
  call foo (cptr, s2)                          ! Check bounds not changed...
  if (s1 .ne. s2) STOP 2! ...and that the descriptor is passed.

  select type (cptr)
    type is (integer)
      if (any (cptr .ne. [1,2,3])) STOP 3! Check the the scalarizer works.
      if (cptr(2) .ne. 2) STOP 4! Check ordinary array indexing.
  end select

  cptr(1:3) => tgt%der%r                       ! Something a tad more complicated!

  select type (cptr)
    type is (real)
      if (any (int(cptr) .ne. [2,4,6])) STOP 5
      if (any (int(cptr([2,3,1])) .ne. [4,6,2])) STOP 6
      if (int(cptr(3)) .ne. 6) STOP 7
  end select

  cptr1(1:3) => tgt%der

  s1 = loc(cptr1)
  call bar(cptr1, s2)
  if (s1 .ne. s2) STOP 8! Check that the descriptor is passed.

  select type (cptr1)
    type is (mytype)
      if (any (cptr1%i .ne. [2,4,6])) STOP 9
      if (cptr1(2)%i .ne. 4) STOP 10
  end select

contains

  subroutine foo (arg, addr)
    class(*), dimension(:), pointer :: arg
    integer(8) :: addr
    addr = loc(arg)
    select type (arg)
      type is (integer)
        if (any (arg .ne. [1,2,3])) STOP 11! Check the the scalarizer works.
        if (arg(2) .ne. 2) STOP 12! Check ordinary array indexing.
    end select
  end subroutine

  subroutine bar (arg, addr)
    class(mytype), dimension(:), pointer :: arg
    integer(8) :: addr
    addr = loc(arg)
    select type (arg)
      type is (mytype)
        if (any (arg%i .ne. [2,4,6])) STOP 13
        if (arg(2)%i .ne. 4) STOP 14
    end select
  end subroutine
end
