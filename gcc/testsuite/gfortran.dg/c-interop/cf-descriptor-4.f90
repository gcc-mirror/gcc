! { dg-do run }
! { dg-additional-sources "cf-descriptor-4-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that building a descriptor for an allocatable
! or pointer array argument in C works and that you can use it to call 
! back into a Fortran function declared to have c binding.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer(C_INT), parameter :: imax=3, jmax=6
end module

subroutine ftest (a, b, initp) bind (c, name="ftest")
  use iso_c_binding
  use mm
  type(m), allocatable :: a(:,:)
  type(m), pointer :: b(:,:)
  integer(C_INT), value :: initp
  integer :: i, j

  if (rank(a) .ne. 2) stop 101
  if (rank(b) .ne. 2) stop 101

  if (initp .ne. 0 .and. .not. allocated(a))  stop 102
  if (initp .eq. 0 .and. allocated(a)) stop 103
  if (initp .ne. 0 .and. .not. associated(b))  stop 104
  if (initp .eq. 0 .and. associated(b)) stop 105

  if (initp .ne. 0) then
    if (lbound (a, 1) .ne. 1) stop 201
    if (lbound (a, 2) .ne. 1) stop 202
    if (lbound (b, 2) .ne. 1) stop 203
    if (lbound (b, 1) .ne. 1) stop 204
    if (ubound (a, 1) .ne. imax) stop 205
    if (ubound (a, 2) .ne. jmax) stop 206
    if (ubound (b, 2) .ne. imax) stop 207
    if (ubound (b, 1) .ne. jmax) stop 208

    do i = 1, imax
      do j = 1, jmax
        if (a(i,j)%i .ne. i) stop 301
        if (a(i,j)%j .ne. j) stop 302
        if (b(j,i)%i .ne. i) stop 303
        if (b(j,i)%j .ne. j) stop 303
      end do
    end do
    
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

  call ctest (imax, jmax)

end program
