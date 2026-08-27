! { dg-do run }
!
! PR125722
!
program p
  type t
     character(2), allocatable :: c(:)
  end type t
  type(t) :: x

  character(:), allocatable :: d(:)
  character(2), allocatable :: e(:)

  ! Deferred-length source longer than the component -> truncate each element.
  allocate (character(5) :: d(3))
  d = [ "abcde", "fghij", "klmno" ]
  x = t (d)
  if (size (x%c) /= 3) stop 1
  if (len (x%c) /= 2) stop 2
  if (x%c(1) /= "ab") stop 3
  if (x%c(2) /= "fg") stop 4
  if (x%c(3) /= "kl") stop 5

  ! Deferred-length source shorter than the component -> blank pad.
  deallocate (d)
  allocate (character(1) :: d(2))
  d = [ "p", "q" ]
  x = t (d)
  if (x%c(1) /= "p ") stop 6
  if (x%c(2) /= "q ") stop 7

  ! Explicit-length allocatable source, equal length.
  allocate (e(2))
  e = [ "xy", "zw" ]
  x = t (e)
  if (x%c(1) /= "xy") stop 8
  if (x%c(2) /= "zw") stop 9
end program p
