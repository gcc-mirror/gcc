! PR 92621 (?)
! { dg-do run }
! { dg-additional-sources "cf-out-descriptor-3-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks that calling a Fortran function with C binding and
! an intent(out) argument works from both C and Fortran.  For this
! test case the argument is an allocatable or pointer scalar.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imagic=-1, jmagic=42

end module

! The call chains being tested here are
!   main -> frob
!   main -> ftest -> frob
!   main -> ctest -> frob
! where everything other than main has C binding.

! frob allocates and initializes its arguments.
! There are two allocatable dummies so that we can pass both
! unallocated (a) and allocated (aa).

subroutine frob (a, aa, p) bind (c, name="frob")
  use iso_c_binding
  use mm
  type(m), intent(out), allocatable :: a, aa
  type(m), intent(out), pointer :: p

  if (allocated (a))  stop 101
  allocate (a)
  a%i = imagic
  a%j = jmagic

  if (allocated (aa))  stop 102
  allocate (aa)
  aa%i = imagic
  aa%j = jmagic

  ! association status of p is undefined on entry
  allocate (p)
  p%i = imagic
  p%j = jmagic
end subroutine

subroutine ftest () bind (c, name="ftest")
  use iso_c_binding
  use mm
  type(m), allocatable :: a, aa
  type(m), pointer :: p

  interface
    subroutine frob (a, aa, p) bind (c, name="frob")
      use iso_c_binding
      use mm
      type(m), intent(out), allocatable :: a, aa
      type(m), intent(out), pointer :: p
    end subroutine
  end interface

  p => NULL ()
  allocate (aa)
  aa%i = 0
  aa%j = 0
  call frob (a, aa, p)

  if (.not. allocated (a)) stop 201
  if (a%i .ne. imagic) stop 202
  if (a%j .ne. jmagic) stop 203

  if (.not. allocated (aa)) stop 204
  if (a%i .ne. imagic) stop 205
  if (a%j .ne. jmagic) stop 206

  if (.not. associated (p)) stop 207
  if (p%i .ne. imagic) stop 208
  if (p%j .ne. jmagic) stop 209

end subroutine

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine frob (a, aa, p) bind (c, name="frob")
      use iso_c_binding
      use mm
      type(m), intent(out), allocatable :: a, aa
      type(m), intent(out), pointer :: p
    end subroutine
    subroutine ftest () bind (c, name="ftest")
      use iso_c_binding
      use mm
    end subroutine
    subroutine ctest (ii, jj) bind (c, name="ctest")
      use iso_c_binding
      use mm
      integer(C_INT), value :: ii, jj
    end subroutine
  end interface

  type(m), allocatable :: a, aa
  type(m), pointer :: p

  p => NULL ()
  allocate (aa)
  aa%i = 0
  aa%j = 0
  call frob (a, aa, p)

  if (.not. allocated (a)) stop 201
  if (a%i .ne. imagic) stop 202
  if (a%j .ne. jmagic) stop 203

  if (.not. allocated (aa)) stop 204
  if (a%i .ne. imagic) stop 205
  if (a%j .ne. jmagic) stop 206

  if (.not. associated (p)) stop 207
  if (p%i .ne. imagic) stop 208
  if (p%j .ne. jmagic) stop 209

  call ftest
  call ctest (imagic, jmagic)

end program
