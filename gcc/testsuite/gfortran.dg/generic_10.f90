! { dg-do compile }
! Test the patch for PR30081 in which non-generic intrinsic
! procedures could not be overloaded by generic interfaces.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de>
!
module gfcbug46
  interface random_seed
     module procedure put_seed
  end interface
  interface random_number
     module procedure random_vector
  end interface
  type t_t
     real :: x(2)
  end type t_t
contains
  subroutine put_seed (n, seed)
    integer, intent(inout) :: n
    integer, intent(in)    :: seed
    call random_seed (size=n)
  end subroutine put_seed
  subroutine random_vector (t)
    type(t_t) :: t
    call random_number (t% x)
  end subroutine random_vector
end module gfcbug46

  use gfcbug46
  type(t_t) :: z
  integer :: n = 2, seed = 1
  call put_seed (n, seed)
  call random_number (z)
  print *, z
end
