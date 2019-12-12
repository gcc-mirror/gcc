! { dg-do run }
!
! Basic tests of SELECT RANK
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  implicit none
  type mytype
    real :: r
  end type
  type, extends(mytype) :: thytype
    integer :: i
  end type

! Torture using integers
ints: block
  integer, dimension(2,2) :: y = reshape ([1,2,3,4],[2,2])
  integer, dimension(4) :: z = [1,2,3,4]
  integer, dimension(2,2,2) :: q = reshape ([11,12,13,14,15,16,17,18],[2,2,2])
  integer :: i = 42

  call ifoo(y, "y")
  if (any (y .ne. reshape ([10,11,12,13], [2,2]))) stop 1
  call ifoo(z, "z")
  call ifoo(i, "i")
  call ifoo(q, "q")
  if (any (q .ne. reshape ([11,12,10,11,15,16,12,13], [2,2,2]))) stop 2
  call ibar(y)
end block ints

! Check derived types
types: block
  integer :: i
  type(mytype), allocatable, dimension(:,:) :: t
  type(mytype), allocatable :: u

  allocate (t, source = reshape ([(mytype(real(i)), i = 1,4)],[2,2]))
  call tfoo(t, "t")
  if (any (size (t) .ne. [1,1])) stop 3   ! 't' has been reallocated!
  if (abs (t(1,1)%r - 42.0) .ge. 1e-6) stop 4
  allocate (u, source = mytype(42.0))
  call tfoo(u, "u")
end block types

! Check classes
classes: block
  integer :: i
  class(mytype), allocatable, dimension(:,:) :: v
  class(mytype), allocatable :: w

  allocate (v, source = reshape ([(mytype(real(i)), i = 1,4)],[2,2]))
  call cfoo(v, "v")
  select type (v)
    type is (mytype)
      stop 5
    type is (thytype)
      if (any (ubound (v) .ne. [3,3])) stop 6
      if (any (abs (v%r - 99.0) .ge. 1e-6)) stop 7
      if (any (v%i .ne. 42)) stop 8
  end select
  allocate (w, source = thytype(42.0, 99))
  call cfoo(w, "w")
end block classes

! Check unlimited polymorphic.
unlimited: block
  integer(4) :: i
  class(*), allocatable, dimension(:,:,:) :: v

  allocate (v, source = reshape ([(i, i = 1,8)],[2,2,2]))
  call ufoo(v, "v")
  select type (v)
    type is (integer(4))
      stop 9
    type is (real(4))
      if (any (ubound(v) .ne. [2,2,1])) stop 10
      if (abs (sum (v) - 10.0) .gt. 1e-6) stop 11
  end select
end block unlimited

contains

  recursive subroutine ifoo(w, chr)
    integer, dimension(..) :: w
    character(1) :: chr

    OUTER: select rank (x => w)
      rank (2)
        if ((chr .eq. 'y') .and. (any (x(1,:) .ne. [1,3]))) stop 12
        if ((chr .eq. 'r') .and. (any (x(1,:) .ne. [13,17]))) stop 13
        x = reshape ([10,11,12,13], [2,2])
      rank (0)
        if ((chr .eq. 'i') .and. (x .ne. 42)) stop 14
      rank (*)
        if ((chr .eq. 'w') .and. (any (x(1:4) .ne. [10,11,12,13]))) stop 15
      rank default
        if ((chr .eq. 'z') .and. (rank (x) .ne. 1)) stop 16
        if ((chr .eq. 'q') .and. (rank (x) .ne. 3)) stop 17
        INNER: select rank (x)
          rank (1) INNER
            if ((chr .eq. 'z') .and. (any (x(1:4) .ne. [1,2,3,4]))) stop 18
          rank (3) INNER
 ! Pass a rank 2 section otherwise an infinite loop ensues.
            call ifoo(x(:,2,:), 'r')
        end select INNER
    end select OUTER
  end subroutine ifoo

  subroutine ibar(x)
    integer, dimension(*) :: x

    call ifoo(x, "w")
  end subroutine ibar

  subroutine tfoo(w, chr)
    type(mytype), dimension(..), allocatable :: w
    character(1) :: chr
    integer :: i
    type(mytype), dimension(2,2) :: r

    select rank (x => w)
      rank (2)
        if (chr .eq. 't') then
          r = reshape ([(mytype(real(i)), i = 1,4)],[2,2])
          if (any (abs (x%r - r%r) .gt. 1e-6)) stop 19
          if (allocated (x)) deallocate (x)
          allocate (x(1,1))
          x(1,1) = mytype (42.0)
        end if
      rank default
        if ((chr .eq. 'u') .and. (rank (x) .ne. 0)) stop 20
    end select
  end subroutine tfoo

  subroutine cfoo(w, chr)
    class(mytype), dimension(..), allocatable :: w
    character(1) :: chr
    integer :: i
    type(mytype), dimension(2,2) :: r

    select rank (c => w)
      rank (2)
        select type (c)
          type is (mytype)
            if (chr .eq. 'v') then
              r = reshape ([(mytype(real(i)), i = 1,4)],[2,2])
              if (any (abs (c%r - r%r) .gt. 1e-6)) stop 21
            end if
          class default
            stop 22
        end select
        if (allocated (c)) deallocate (c)
        allocate (c(3,3), source = thytype (99.0, 42))
      rank default
        if ((chr .eq. 'w') .and. (rank (c) .ne. 0)) stop 23
    end select
  end subroutine cfoo

  subroutine ufoo(w, chr)
    class(*), dimension(..), allocatable :: w
    character(1) :: chr
    integer :: i

    select rank (c => w)
      rank (3)
        select type (c)
          type is (integer(4))
            if (chr .eq. 'v' .and. (sum (c) .ne. 36)) stop 24
          class default
            stop 25
        end select
        if (allocated (c)) deallocate(c)
        allocate (c, source = reshape ([(real(i), i = 1,4)],[2,2,1]))
      rank default
        stop 26
    end select
  end subroutine ufoo

end
