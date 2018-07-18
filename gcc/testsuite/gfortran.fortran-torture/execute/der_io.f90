! Program to test IO of derived types
program derived_io
  character(400) :: buf1, buf2, buf3

  type xyz_type
     integer :: x
     character(11) :: y
     logical :: z
  end type xyz_type

  type abcdef_type
     integer :: a
     logical :: b
     type (xyz_type) :: c
     integer :: d
     real(4) :: e
     character(11) :: f
  end type abcdef_type

  type (xyz_type), dimension(2) :: xyz
  type (abcdef_type) abcdef

  xyz(1)%x = 11111
  xyz(1)%y = "hello world"
  xyz(1)%z = .true.
  xyz(2)%x = 0
  xyz(2)%y = "go away"
  xyz(2)%z = .false.

  abcdef%a = 0
  abcdef%b = .true.
  abcdef%c%x = 111
  abcdef%c%y = "bzz booo"
  abcdef%c%z = .false.
  abcdef%d = 3
  abcdef%e = 4.0
  abcdef%f = "kawabanga"

  write (buf1, *), xyz(1)%x, xyz(1)%y, xyz(1)%z
  ! Use function call to ensure it is only evaluated once
  write (buf2, *), xyz(bar())
  if (buf1.ne.buf2) STOP 1

  write (buf1, *), abcdef
  write (buf2, *), abcdef%a, abcdef%b, abcdef%c, abcdef%d, abcdef%e, abcdef%f
  write (buf3, *), abcdef%a, abcdef%b, abcdef%c%x, abcdef%c%y, &
                   abcdef%c%z, abcdef%d, abcdef%e, abcdef%f
  if (buf1.ne.buf2) STOP 2
  if (buf1.ne.buf3) STOP 3

  call foo(xyz(1))

  contains

    subroutine foo(t)
      type (xyz_type) t
      write (buf1, *), t%x, t%y, t%z
      write (buf2, *), t
      if (buf1.ne.buf2) STOP 4
    end subroutine foo

    integer function bar()
      integer, save :: i = 1
      bar = i
      i = i + 1
    end function
end
