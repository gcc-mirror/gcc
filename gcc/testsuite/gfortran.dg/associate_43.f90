! { dg-do run }
!
! Check that PR83146 remains fixed.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
  type foo
    integer n
  end type
  type bar
    type(foo) array(2)
  end type

  type(bar) b
  integer :: m=0

  b%array(1)%n = 42
  b%array(2)%n = 43

  call assoc(1)
  m = 1
  call assoc(2)
contains
  subroutine assoc (n)
    integer :: n
    associate (n_array => b%array%n)
      select case (n_array(n))
        case (42)
          if (m .ne. 0) stop 1
      case default
          if (m .eq. 0) stop 2
      end select
    end associate
  end subroutine assoc
end