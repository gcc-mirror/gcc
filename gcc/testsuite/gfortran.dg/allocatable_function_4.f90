! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/37626
! Contributed by Rich Townsend
!
! The problem was an ICE when trying to deallocate the
! result variable "x_unique".
!
function unique_A (x, sorted) result (x_unique)
  implicit none
  character(*), dimension(:), intent(in)       :: x
  logical, intent(in), optional                :: sorted
  character(LEN(x)), dimension(:), allocatable :: x_unique

  logical                                      :: sorted_
  character(LEN(x)), dimension(SIZE(x))        :: x_sorted
  integer                                      :: n_x
  logical, dimension(SIZE(x))                  :: mask

  integer, external                            :: b3ss_index

! Set up sorted_

  if(PRESENT(sorted)) then
     sorted_ = sorted
  else
     sorted_ = .FALSE.
  endif

! If necessary, sort x

  if(sorted_) then
     x_sorted = x
  else
     x_sorted = x(b3ss_index(x))
  endif

! Set up the unique array

  n_x = SIZE(x)

  mask = (/.TRUE.,x_sorted(2:n_x) /= x_sorted(1:n_x-1)/)

  allocate(x_unique(COUNT(mask)))

  x_unique = PACK(x_sorted, MASK=mask)

! Finish

  return
end function unique_A

! { dg-final { scan-tree-dump-times "__builtin_free" 5 "original" } }
! { dg-final { cleanup-tree-dump "original" } }

