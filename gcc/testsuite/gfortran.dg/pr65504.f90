! PR target/65504
! { dg-do run }

program pr65504
  implicit none
  type :: T
    character (len=256) :: a
    character (len=256) :: b
  end type T
  type (T) :: c
  type (T) :: d
  c = foo ("test")
  d = foo ("test")
  if (trim(c%b) .ne. "foo") STOP 1
  contains
  type (T) function foo (x) result (v)
    character(len=*), intent(in) :: x
    select case (x)
    case ("test")
      v%b = 'foo'
    case ("bazx")
      v%b = 'barx'
    case default
      print *, "unknown"
      stop
    end select
  end function foo
end program pr65504
