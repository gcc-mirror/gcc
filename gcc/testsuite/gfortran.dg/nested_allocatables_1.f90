! { dg-do run }
!
! PR fortran/40850
! The code freeing allocatable components used to be put after the code
! freeing the containing entity.
!
! Original test case by Marco Restelli <mrestelli@gmail.com>
! Reduced by Daniel Franke <franke.daniel@gmail.com>
!        and Janus Weil <janus@gcc.gnu.org>


  type t
    integer, allocatable :: d(:)
  end type
  type(t), allocatable :: a(:)

  ! Big enough to make it fail
  allocate(a(2 * 1024))
  call sub( (/ a /) )

contains

  subroutine sub(b)
    type(t) :: b(:)
  end subroutine

end

