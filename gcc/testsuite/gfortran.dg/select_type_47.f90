! { dg-do compile }
!
! PR fortran/87632
!
! Contributed by Jürgen Reuter
!
module m
type t
  integer :: i
end type t
type t2
  type(t) :: phs_config
end type t2
end module m

module m2
use m
implicit none
type t3
end type t3

type process_t
  private
  type(t2), allocatable :: component(:)
contains
  procedure :: get_phs_config => process_get_phs_config
end type process_t

contains
  subroutine process_extract_resonance_history_set &
       (process, include_trivial, i_component)
    class(process_t), intent(in), target :: process
    logical, intent(in), optional :: include_trivial
    integer, intent(in), optional :: i_component
    integer :: i
    i = 1;  if (present (i_component))  i = i_component
    select type (phs_config => process%get_phs_config (i))
    class is (t)
       call foo()
    class default
       call bar()
    end select
  end subroutine process_extract_resonance_history_set

  function process_get_phs_config (process, i_component) result (phs_config)
    class(t), pointer :: phs_config
    class(process_t), intent(in), target :: process
    integer, intent(in) :: i_component
    if (allocated (process%component)) then
       phs_config => process%component(i_component)%phs_config
    else
       phs_config => null ()
    end if
  end function process_get_phs_config
end module m2

program main
  use m2
end program main
