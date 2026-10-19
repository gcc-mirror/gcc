! { dg-do run }
!
! PR 125720
!
! A structure constructor that assigns an allocatable or deferred-length
! character expression to an explicit-length allocatable character component
! used to ICE in gfc_trans_subcomponent_assign (fold_convert_loc).  Check that
! it now compiles and copies with the correct truncation/padding.

program p
  type t
     integer :: i
     character(2), allocatable :: c
  end type t
  type(t) :: x

  character(2), allocatable :: a
  character(:), allocatable :: d

  ! Allocatable, explicit-length source.
  allocate (a)
  a = "cd"
  x = t (2, a)
  if (x%i /= 2) stop 1
  if (.not. allocated (x%c)) stop 2
  if (len (x%c) /= 2) stop 3
  if (x%c /= "cd") stop 4

  ! Deferred-length source, exact length.
  d = "ef"
  x = t (3, d)
  if (x%c /= "ef") stop 5

  ! Deferred-length source, longer than the component -> truncated.
  d = "ghij"
  x = t (4, d)
  if (x%c /= "gh") stop 6

  ! Deferred-length source, shorter than the component -> blank padded.
  d = "k"
  x = t (5, d)
  if (x%c /= "k ") stop 7
end program p
