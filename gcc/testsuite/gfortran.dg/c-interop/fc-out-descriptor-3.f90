! PR 101308
! { dg-do run }
! { dg-additional-sources "fc-out-descriptor-3-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that passing an allocatable or pointer scalar
! as an intent(out) argument to a C function called from Fortran works.  

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer(C_INT), parameter :: iinit = 42, jinit = 12345

end module

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest1 (ii, jj, p) bind (c)
      use iso_c_binding
      use mm
      integer(C_INT), value :: ii, jj
      type(m), intent(out), pointer :: p
    end subroutine
    subroutine ctest2 (ii, jj, a) bind (c)
      use iso_c_binding
      use mm
      integer(C_INT), value :: ii, jj
      type(m), intent(out), allocatable :: a
    end subroutine
  end interface

  type(m), pointer :: p
  type(m), allocatable :: a

  ! The association status of the intent(out) pointer argument is supposed
  ! to become undefined on entry to the called procedure.  
  p => NULL ()
  call ctest1 (iinit, jinit, p)
  if (.not. associated (p)) stop 101
  if (p%i .ne. iinit) stop 102
  if (p%j .ne. jinit) stop 103

  ! The intent(out) argument is supposed to be deallocated automatically
  ! on entry to the called function.
  allocate (a)
  a%i = 0
  a%j = 0
  call ctest2 (iinit, jinit, a)
  if (.not. allocated (a)) stop 201
  if (a%i .ne. iinit) stop 202
  if (a%j .ne. jinit) stop 203
end program
