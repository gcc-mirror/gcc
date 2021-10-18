! PR 101308
! PR 92621(?)
! { dg-do run }
! { dg-additional-sources "allocatable-dummy-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! TS 29113
! 6.3 Argument association
!
! When a Fortran procedure that has an INTENT(OUT) allocatable dummy
! argument is invoked by a C function, and the actual argument in the C
! function is the address of a C descriptor that describes an allocated
! allocatable variable, the variable is deallocated on entry to the
! Fortran procedure.  

! When a C function is invoked from a Fortran procedure via an interface
! with an INTENT(OUT) allocatable dummy argument, and the actual
! argument in the reference to the C function is an allocated
! allocatable variable, the variable is deallocated on invocation
! (before execution of the C function begins).

module m
  use iso_c_binding

  type, bind (c) :: t
    real(C_FLOAT) :: xyz(3)
    integer(C_INT) :: id
  end type

  interface
    subroutine testit_c (a, x, y, z) bind (c)
      use iso_c_binding
      import :: t
      type (t), allocatable, intent(out) :: a
      real(C_FLOAT), value, intent(in) :: x, y, z
    end subroutine
  end interface

  contains

    subroutine testit_f (a, x, y, z)
      type (t), allocatable, intent(out) :: a
      real(C_FLOAT), value, intent(in) :: x, y, z
      if (allocated (a))  stop 201
      allocate (a)
      a%id = 69
      a%xyz(1) = x
      a%xyz(2) = y
      a%xyz(3) = z
    end subroutine

    subroutine testit_f_bind_c (a, x, y, z) bind (c)
      type (t), allocatable, intent(out) :: a
      real(C_FLOAT), value, intent(in) :: x, y, z
      if (allocated (a))  stop 301
      allocate (a)
      a%id = -1
      a%xyz(1) = x
      a%xyz(2) = y
      a%xyz(3) = z
    end subroutine

end module

program test
  use iso_c_binding
  use m

  type (t), allocatable :: b

  if (allocated (b))  stop 401

  ! Try the regular Fortran test routine.
  allocate (b)
  call testit_f (b, 1.0, 2.0, 3.0)
  if (.not. allocated (b))  stop 402
  deallocate (b)
  if (allocated (b))  stop 403

  ! Try the test routine written in Fortran with C binding.
  allocate (b)
  call testit_f_bind_c (b, 1.0, 2.0, 3.0)
  if (.not. allocated (b))  stop 404
  deallocate (b)
  if (allocated (b))  stop 405

  ! Try the test routine written in C.  This calls testit_f_bind_c
  ! before returning, so make sure that's what we've got when returning.
  allocate (b)
  call testit_c (b, -1.0, -2.0, -3.0)
  if (.not. allocated (b))  stop 406
  if (b%id .ne. -1)  stop 407
  if (b%xyz(1) .ne. -1.0)  stop 408
  if (b%xyz(2) .ne. -2.0)  stop 408
  if (b%xyz(3) .ne. -3.0)  stop 408
  deallocate (b)

end program
