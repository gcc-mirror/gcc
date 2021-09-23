! { dg-do run }
! { dg-additional-sources "cf-descriptor-3-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that building a descriptor for an allocatable
! or pointer scalar argument in C works and that you can use it to call 
! back into a Fortran function declared to have c binding.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer(C_INT), parameter :: imagic = 42, jmagic = 69
end module

subroutine ftest (a, b, initp) bind (c, name="ftest")
  use iso_c_binding
  use mm
  type(m), allocatable :: a
  type(m), pointer :: b
  integer(C_INT), value :: initp

  if (rank(a) .ne. 0) stop 101
  if (rank(b) .ne. 0) stop 101

  if (initp .ne. 0 .and. .not. allocated(a))  stop 102
  if (initp .eq. 0 .and. allocated(a)) stop 103
  if (initp .ne. 0 .and. .not. associated(b))  stop 104
  if (initp .eq. 0 .and. associated(b)) stop 105

  if (initp .ne. 0) then
    if (a%i .ne. imagic) stop 201
    if (a%j .ne. jmagic) stop 202
    if (b%i .ne. imagic + 1) stop 203
    if (b%j .ne. jmagic + 1) stop 204
  end if
end subroutine


program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest (i, j) bind (c)
      use iso_c_binding
      integer(C_INT), value :: i, j
    end subroutine
  end interface

  ! ctest will call ftest with both an unallocated and allocated argument.

  call ctest (imagic, jmagic)

end program
