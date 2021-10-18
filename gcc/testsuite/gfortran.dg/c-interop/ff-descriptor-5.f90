! PR92482
! { dg-do run }
!
! This program checks that passing arrays as assumed-length character
! dummies to and from Fortran functions with C binding works.

program testit
  use iso_c_binding
  implicit none

  character(len=26,kind=C_CHAR) :: aa

  call testc (aa)
  call testf (aa)

contains

  ! C binding version

  subroutine checkc (a) bind (c)
    use iso_c_binding
    character(len=*,kind=C_CHAR) :: a

    if (rank (a) .ne. 0) stop 101
    if (len (a) .ne. 26) stop 102
    if (a .ne. 'abcdefghijklmnopqrstuvwxyz') stop 103
  end subroutine

  ! Fortran binding version
  subroutine checkf (a)
    use iso_c_binding
    character(len=*,kind=C_CHAR) :: a

    if (rank (a) .ne. 0) stop 201
    if (len (a) .ne. 26) stop 202
    if (a .ne. 'abcdefghijklmnopqrstuvwxyz') stop 203
  end subroutine

  ! C binding version
  subroutine testc (a) bind (c)
    use iso_c_binding
    character(len=*,kind=C_CHAR) :: a

    ! Call both the C and Fortran binding check functions
    a = 'abcdefghijklmnopqrstuvwxyz'
    call checkc (a)
    call checkf (a)
  end subroutine

  ! Fortran binding version
  subroutine testf (a)
    use iso_c_binding
    character(len=*,kind=C_CHAR) :: a

    ! Call both the C and Fortran binding check functions
    a = 'abcdefghijklmnopqrstuvwxyz'
    call checkc (a)
    call checkf (a)
  end subroutine

end program
