! { dg-do compile }
!
! PR 54189: ICE (segfault) with invalid assumed-size dummy
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

  implicit none
  procedure(g), pointer :: x  ! { dg-error "must be a dummy argument" }
  x => g

contains

  function g()        ! { dg-error "must be a dummy argument" }
    integer :: g(*)
  end function

end
