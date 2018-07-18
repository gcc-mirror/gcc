! { dg-do run }
!
! Test for the fix for PR34640. In this case, final testing of the
! patch revealed that in some cases the actual descriptor was not
! being passed to procedure dummy pointers.
!
! Contributed by Thomas Koenig  <tkoenig@netcologne.de>
!
module x
  use iso_c_binding
  implicit none
  type foo
     complex :: c
     integer :: i
  end type foo
contains
  subroutine printit(c, a)
    complex, pointer, dimension(:) :: c
    integer :: i
    integer(kind=c_intptr_t) :: a
    a = transfer(c_loc(c(2)),a)
  end subroutine printit
end module x

program main
  use x
  use iso_c_binding
  implicit none
  type(foo), dimension(5), target :: a
  integer :: i
  complex, dimension(:), pointer :: pc
  integer(kind=c_intptr_t) :: s1, s2, s3
  a%i = 0
  do i=1,5
     a(i)%c = cmplx(i**2,i)
  end do
  pc => a%c
  call printit(pc, s3)

  s1 = transfer(c_loc(a(2)%c),s1)
  if (s1 /= s3) STOP 1

  s2 = transfer(c_loc(pc(2)),s2)
  if (s2 /= s3) STOP 2

end program main
