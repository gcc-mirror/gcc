! { dg-do run }
!
! PR fortran/57445
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
! Spurious assert was added at revision 192495
!
module m
  implicit none
  type t
    integer :: i
  end type t
contains
  subroutine opt(xa, xc, xaa, xca)
    type(t),  allocatable, intent(out), optional :: xa
    class(t), allocatable, intent(out), optional :: xc
    type(t),  allocatable, intent(out), optional :: xaa(:)
    class(t), allocatable, intent(out), optional :: xca(:)
    if (present (xca)) call foo_opt(xca=xca)
  end subroutine opt
  subroutine foo_opt(xa, xc, xaa, xca)
    type(t),  allocatable, intent(out), optional :: xa
    class(t), allocatable, intent(out), optional :: xc
    type(t),  allocatable, intent(out), optional :: xaa(:)
    class(t), allocatable, intent(out), optional :: xca(:)
    if (present (xca)) then
      if (allocated (xca)) deallocate (xca)
      allocate (xca(3), source = [t(9),t(99),t(999)])
    end if
  end subroutine foo_opt
end module m
  use m
  class(t), allocatable :: xca(:)
  allocate (xca(1), source = t(42))
  select type (xca)
    type is (t)
      if (any (xca%i .ne. [42])) call abort
  end select
  call opt (xca = xca)
  select type (xca)
    type is (t)
      if (any (xca%i .ne. [9,99,999])) call abort
  end select
end
