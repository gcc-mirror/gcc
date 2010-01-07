! { dg-do run }
!
! PR fortran/41872
!
! Allocatable scalars with SAVE
!
program test
  implicit none
  call sub (0)
  call sub (1)
  call sub (2)
contains
  subroutine sub (no)
    integer, intent(in) :: no
    integer, allocatable, save :: a
    if (no == 0) then
      if (allocated (a)) call abort ()
      allocate (a)
    else if (no == 1) then
      if (.not. allocated (a)) call abort ()
      deallocate (a)
    else
      if (allocated (a)) call abort ()
    end if
  end subroutine sub
end program test
