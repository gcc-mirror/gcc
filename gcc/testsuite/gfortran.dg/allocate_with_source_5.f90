! { dg-do run }
!
! Check that pr65548 is fixed.
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>

module allocate_with_source_5_module

  type :: selector_t
    integer, dimension(:), allocatable :: map
    real, dimension(:), allocatable :: weight
  contains
    procedure :: init => selector_init
  end type selector_t

contains

  subroutine selector_init (selector, weight)
    class(selector_t), intent(out) :: selector
    real, dimension(:), intent(in) :: weight
    real :: s
    integer :: n, i
    logical, dimension(:), allocatable :: mask
    s = sum (weight)
    allocate (mask (size (weight)), source = weight /= 0)
    n = count (mask)
    if (n > 0) then
       allocate (selector%map (n), &
            source = pack ([(i, i = 1, size (weight))], mask))
       allocate (selector%weight (n), &
            source = pack (weight / s, mask))
    else
       allocate (selector%map (1), source = 1)
       allocate (selector%weight (1), source = 0.)
    end if
  end subroutine selector_init

end module allocate_with_source_5_module

program allocate_with_source_5
  use allocate_with_source_5_module

  class(selector_t), allocatable :: sel;
  real, dimension(5) :: w = [ 1, 0, 2, 0, 3];

  allocate (sel)
  call sel%init(w)

  if (any(sel%map /= [ 1, 3, 5])) call abort()
  if (any(abs(sel%weight - [1, 2, 3] / 6) < 1E-6)) call abort()
end program allocate_with_source_5
! { dg-final { cleanup-modules "allocate_with_source_5_module" } }

