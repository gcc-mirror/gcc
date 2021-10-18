! { dg-do run }
!
! This program checks that passing allocatable and pointer arrays to
! and from Fortran functions with C binding works.

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type
end module

program testit
  use iso_c_binding
  use mm
  implicit none

  type(m), allocatable :: a(:)
  type(m), target :: t(3,10)
  type(m), pointer :: p(:,:)

  p => NULL()

  call testc (a, t, p)
  call testf (a, t, p)

contains

  ! C binding version

  subroutine checkc (a, t, p, initp) bind (c)
    use iso_c_binding
    use mm
    type(m), allocatable :: a(:)
    type(m), target :: t(3,10)
    type(m), pointer :: p(:,:)
    logical, value :: initp
    integer :: i, j

    if (rank (a) .ne. 1) stop 101
    if (rank (t) .ne. 2) stop 102
    if (rank (p) .ne. 2) stop 103

    if (initp) then
      if (.not. allocated (a)) stop 104
      if (.not. associated (p)) stop 105
      if (.not. associated (p, t)) stop 106
      if (size (a, 1) .ne. 5) stop 107
      if (size (p, 1) .ne. 3) stop 108
      if (size (p, 2) .ne. 10) stop 109
    else
      if (allocated (a)) stop 121
      if (associated (p)) stop 122
    end if

  end subroutine

  ! Fortran binding version
  subroutine checkf (a, t, p, initp)
    use iso_c_binding
    use mm
    type(m), allocatable :: a(:)
    type(m), target :: t(3,10)
    type(m), pointer :: p(:,:)
    logical, value :: initp
    integer :: i, j

    if (rank (a) .ne. 1) stop 201
    if (rank (t) .ne. 2) stop 202
    if (rank (p) .ne. 2) stop 203

    if (initp) then
      if (.not. allocated (a)) stop 204
      if (.not. associated (p)) stop 205
      if (.not. associated (p, t)) stop 206
      if (size (a, 1) .ne. 5) stop 207
      if (size (p, 1) .ne. 3) stop 208
      if (size (p, 2) .ne. 10) stop 209
    else
      if (allocated (a)) stop 221
      if (associated (p)) stop 222
    end if

  end subroutine

  ! C binding version
  subroutine allocatec (a, t, p) bind (c)
    use iso_c_binding
    use mm
    type(m), allocatable :: a(:)
    type(m), target :: t(3,10)
    type(m), pointer :: p(:,:)

    allocate (a(10:20))
    p => t
  end subroutine

  ! Fortran binding version
  subroutine allocatef (a, t, p) bind (c)
    use iso_c_binding
    use mm
    type(m), allocatable :: a(:)
    type(m), target :: t(3,10)
    type(m), pointer :: p(:,:)

    allocate (a(5:15))
    p => t
  end subroutine

  ! C binding version
  subroutine testc (a, t, p) bind (c)
    use iso_c_binding
    use mm
    type(m), allocatable :: a(:)
    type(m), target :: t(3,10)
    type(m), pointer :: p(:,:)

    ! Call both the C and Fortran binding check functions
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

    ! Allocate/associate and check again.
    allocate (a(5))
    p => t
    call checkc (a, t, p, .true.)
    call checkf (a, t, p, .true.)

    ! Reset and check a third time.
    deallocate (a)
    p => NULL ()
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

    ! Allocate/associate inside a function with Fortran binding.
    call allocatef (a, t, p)
    if (.not. allocated (a)) stop 301
    if (.not. associated (p)) stop 302
    if (lbound (a, 1) .ne. 5) stop 303
    if (ubound (a, 1) .ne. 15) stop 304
    deallocate (a)
    p => NULL ()

    ! Allocate/associate inside a function with C binding.
    call allocatec (a, t, p)
    if (.not. allocated (a)) stop 311
    if (.not. associated (p)) stop 312
    if (lbound (a, 1) .ne. 10) stop 313
    if (ubound (a, 1) .ne. 20) stop 314
    deallocate (a)
    p => NULL ()

  end subroutine

  ! Fortran binding version
  subroutine testf (a, t, p)
    use iso_c_binding
    use mm
    type(m), allocatable :: a(:)
    type(m), target :: t(3,10)
    type(m), pointer :: p(:,:)

    ! Call both the C and Fortran binding check functions
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

    ! Allocate/associate and check again.
    allocate (a(5))
    p => t
    call checkc (a, t, p, .true.)
    call checkf (a, t, p, .true.)

    ! Reset and check a third time.
    deallocate (a)
    p => NULL ()
    call checkc (a, t, p, .false.)
    call checkf (a, t, p, .false.)

    ! Allocate/associate inside a function with Fortran binding.
    call allocatef (a, t, p)
    if (.not. allocated (a))  stop 401
    if (.not. associated (p)) stop 402
    if (lbound (a, 1) .ne. 5) stop 403
    if (ubound (a, 1) .ne. 15) stop 404
    deallocate (a)
    p => NULL ()

    ! Allocate/associate inside a function with C binding.
    call allocatec (a, t, p)
    if (.not. allocated (a))  stop 411
    if (.not. associated (p)) stop 412
    if (lbound (a, 1) .ne. 10) stop 413
    if (ubound (a, 1) .ne. 20) stop 414
    deallocate (a)
    p => NULL ()

  end subroutine

end program
