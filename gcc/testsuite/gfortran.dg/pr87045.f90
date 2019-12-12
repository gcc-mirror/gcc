! { dg-do run }
! { dg-additional-options "-fcheck=bounds" }
!
! PR fortran/87045 - pointer to array of character
! Contributed by Valery Weber
! This used to give an invalid run-time error

program test
  character(:), dimension(:), allocatable, target :: t
  character(:), pointer, dimension(:) :: p
  allocate( character(3) :: t(2) )
  t(1) = "abc"
  t(2) = "123"
  p => t
  if (size (p) /= 2) stop 1
  if (len  (p) /= 3) stop 2
  if (p(1) /= "abc") stop 3
  if (p(2) /= "123") stop 4
end program test
