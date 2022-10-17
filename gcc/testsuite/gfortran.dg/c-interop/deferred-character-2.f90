! PR 92482
! { dg-do execute}
!
! TS 29113
! 8.7 Interoperability of procedures and procedure interfaces
! 
! If a dummy argument in an interoperable interface is of type
! CHARACTER and is allocatable or a pointer, its character length shall
! be deferred.

program testit
  use iso_c_binding

  character (kind=C_CHAR, len=:), allocatable :: aa
  character (kind=C_CHAR, len=:), pointer :: pp


  pp => NULL ()

  call frobf (aa, pp)
  if (.not. allocated (aa)) stop 101
  if (aa .ne. 'foo') stop 102
  if (.not. associated (pp)) stop 103
  if (pp .ne. 'bar') stop 104

  pp => NULL ()

  call frobc (aa, pp)
  if (.not. allocated (aa)) stop 101
  if (aa .ne. 'frog') stop 102
  if (.not. associated (pp)) stop 103
  if (pp .ne. 'toad') stop 104


  contains

    subroutine frobf (a, p)
      use iso_c_binding
      character (kind=C_CHAR, len=:), allocatable :: a
      character (kind=C_CHAR, len=:), pointer :: p
      allocate (character(len=3) :: p)
      a = 'foo'
      p = 'bar'
    end subroutine

    subroutine frobc (a, p) bind (c)
      use iso_c_binding
      character (kind=C_CHAR, len=:), allocatable :: a
      character (kind=C_CHAR, len=:), pointer :: p
      allocate (character(len=4) :: p)
      a = 'frog'
      p = 'toad'
    end subroutine

end program
