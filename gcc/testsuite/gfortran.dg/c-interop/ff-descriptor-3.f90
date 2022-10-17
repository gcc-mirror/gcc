! { dg-do run }
!
! This program checks that passing allocatable and pointer scalars to
! and from Fortran functions with C binding works.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imagic=-1, jmagic=42
end module

program testit
  use iso_c_binding
  use mm
  implicit none

  type(m), allocatable :: a
  type(m), target :: t
  type(m), pointer :: p

  p => NULL()

  call testc (a, t, p)
  call testf (a, t, p)

contains

  ! C binding version

  subroutine checkc (a, t, p, initp) bind (c)
    use iso_c_binding
    use mm
    type(m), allocatable :: a
    type(m), target :: t
    type(m), pointer :: p
    logical, value :: initp

    if (initp) then
      if (.not. allocated (a)) stop 101
      if (a%i .ne. imagic) stop 102
      if (a%j .ne. jmagic) stop 103
      if (.not. associated (p)) stop 104
      if (.not. associated (p, t)) stop 105
      if (p%i .ne. imagic) stop 106
      if (p%j .ne. jmagic) stop 107
    else
      if (allocated (a)) stop 108
      if (associated (p)) stop 109
    end if

    if (rank (a) .ne. 0) stop 110
    if (rank (t) .ne. 0) stop 111
    if (rank (p) .ne. 0) stop 112

  end subroutine

  ! Fortran binding version
  subroutine checkf (a, t, p, initp)
    use iso_c_binding
    use mm
    type(m), allocatable :: a
    type(m), target :: t
    type(m), pointer :: p
    logical, value :: initp

    if (initp) then
      if (.not. allocated (a)) stop 201
      if (a%i .ne. imagic) stop 202
      if (a%j .ne. jmagic) stop 203
      if (.not. associated (p)) stop 204
      if (.not. associated (p, t)) stop 205
      if (p%i .ne. imagic) stop 206
      if (p%j .ne. jmagic) stop 207
    else
      if (allocated (a)) stop 208
      if (associated (p)) stop 209
    end if

    if (rank (a) .ne. 0) stop 210
    if (rank (t) .ne. 0) stop 211
    if (rank (p) .ne. 0) stop 212

  end subroutine

  ! C binding version
  subroutine testc (a, t, p) bind (c)
    use iso_c_binding
    use mm
    type(m), allocatable :: a
    type(m), target :: t
    type(m), pointer :: p

    ! Call both the C and Fortran binding check functions
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

    ! Allocate/associate and check again.
    allocate (a)
    a%i = imagic
    a%j = jmagic
    p => t
    t%i = imagic
    t%j = jmagic
    call checkc (a, t, p, .true.)
    call checkf (a, t, p, .true.)

    ! Reset and check a third time.
    deallocate (a)
    p => NULL ()
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

  end subroutine

  ! Fortran binding version
  subroutine testf (a, t, p)
    use iso_c_binding
    use mm
    type(m), allocatable :: a
    type(m), target :: t
    type(m), pointer :: p

    ! Call both the C and Fortran binding check functions
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

    ! Allocate/associate and check again.
    allocate (a)
    a%i = imagic
    a%j = jmagic
    p => t
    t%i = imagic
    t%j = jmagic
    call checkc (a, t, p, .true.)
    call checkf (a, t, p, .true.)

    ! Reset and check a third time.
    deallocate (a)
    p => NULL ()
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

  end subroutine

end program
