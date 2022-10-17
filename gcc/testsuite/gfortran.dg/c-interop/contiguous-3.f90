! PR 101304
! { dg-do run }
! { dg-additional-sources "contiguous-3-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! TS 29113
! 8.7 In an invocation of an interoperable procedure whose Fortran
! interface has an assumed-shape or assumed-rank dummy argument with the
! CONTIGUOUS attribute, the associated effective argument may be an
! array that is not contiguous or the address of a C descriptor for such
! an array. If the procedure is invoked from Fortran or the procedure is
! a Fortran procedure, the Fortran processor will handle the difference
! in contiguity. If the procedure is invoked from C and the procedure is
! a C procedure, the C code within the procedure shall be prepared to
! handle the situation of receiving a discontiguous argument.
!
! The wording is different in the 2018 standard, but the intent is more
! or less the same:
!
! When an interoperable Fortran procedure that is invoked from C has a 
! dummy argument with the CONTIGUOUS attribute or that is an assumed-length 
! CHARACTER explicit-shape or assumed-size array, and the actual argument 
! is the address of a C descriptor for a discontiguous object, the Fortran 
! processor shall handle the difference in contiguity.
!
! This program tests the cases where a Fortran procedure with C binding and
! a dummy array argument with the contiguous attribute is invoked from
! both C or Fortran.  It is similar to contiguous-2.f90 but here the array
! sections are created in Fortran even in the called-from-C case, rather
! than by calling CFI_section.

! ftest1 and ftest2 both negate the elements of their input array;
! this allows testing that modifications to the array contents get
! propagated back to the base array.

module m

  contains

  subroutine ftest1 (a, first, last, step) bind (c)
    use iso_c_binding
    integer(C_INT), contiguous :: a(:)
    integer(C_INT), value :: first, last, step
    integer :: i, ival

    ! Sanity checking that we got a contiguous array.  The direct call
    ! to is_contiguous might be optimized away, but the indirect one
    ! in check_contiguous shouldn't be.
    ! FIXME: is this correct?  "the Fortran processor will handle the
    ! difference in contiguity" may not mean that it's required to make
    ! the array contiguous, just that it can access it correctly?
    if (.not. is_contiguous (a)) stop 301
    call check_contiguous (a)

    ! Sanity checking that we got the right input array contents.
    ! print *, 'a on entry to ftest1'
    ! do i = lbound(a, 1), ubound(a, 1)
    !   print *, 'a(', i, ') = ', a(i)
    ! end do
    ival = first
    do i = lbound(a, 1), ubound(a, 1)
      if (a (i) .ne. ival) then
        print *, 'a(', i, ') = ', a(i), '  expected ', ival
        stop 302
      end if
      a(i) = - a(i)
      ival = ival + step
    end do
  end subroutine

  subroutine ftest2 (a, first, last, step) bind (c)
    use iso_c_binding

    integer(C_INT), contiguous :: a(..)
    integer(C_INT), value :: first, last, step

    select rank (a)
      rank (1)
        call ftest1 (a(:), first, last, step)
      rank default
        stop 303
    end select
  end subroutine

  subroutine check_contiguous (a)
    use iso_c_binding
    integer(C_INT) :: a(..)
    if (.not. is_contiguous (a)) stop 304
  end subroutine

end module


program testit
  use iso_c_binding
  use m
  implicit none

  ! Note ctest1 and ctest2 do not have the contiguous attribute on a.
  interface
    subroutine ctest1 (a, first, last, step) bind (c)
      use iso_c_binding
      integer(C_INT) :: a(:)
      integer(C_INT), value :: first, last, step
    end subroutine
    subroutine ctest2 (a, first, last, step) bind (c)
      use iso_c_binding
      integer(C_INT) :: a(..)
      integer(C_INT), value :: first, last, step
    end subroutine
  end interface

  integer(C_INT) :: aa(32)
  integer :: i

  ! assumed-shape, called from Fortran
  do i = 1, 32
    aa(i) = i
  end do
  call ftest1 (aa(4:12:2), 4, 12, 2)
  do i = 1, 32
    if (i .ge. 4 .and. i .le. 12 .and. mod (i-4,2) .eq. 0) then
      if (aa (i) .ne. -i) stop 101
    else
      if (aa (i) .ne. i) stop 102
    end if
  end do

  ! assumed-shape, called indirectly from C code, using an array
  ! section created in Fortran instead of by CFI_section
  do i = 1, 32
    aa(i) = i
  end do
  call ctest1 (aa(5:13:2), 5, 13, 2)
  do i = 1, 32
    if (i .ge. 5 .and. i .le. 13 .and. mod (i-5,2) .eq. 0) then
      if (aa (i) .ne. -i) stop 103
    else
      if (aa (i) .ne. i) stop 104
    end if
  end do

  ! assumed-rank, called from Fortran
  do i = 1, 32
    aa(i) = i
  end do
  call ftest2 (aa(7:19:3), 7, 19, 3)
  do i = 1, 32
    if (i .ge. 7 .and. i .le. 19 .and. mod (i-7,3) .eq. 0) then
      if (aa (i) .ne. -i) stop 201
    else
      if (aa (i) .ne. i) stop 202
    end if
  end do

  ! assumed-rank, called indirectly from C code, using an array
  ! section created in Fortran instead of by CFI_section
  do i = 1, 32
    aa(i) = i
  end do
  call ctest2 (aa(8:20:3), 8, 20, 3)
  do i = 1, 32
    if (i .ge. 8 .and. i .le. 20 .and. mod (i-8,3) .eq. 0) then
      if (aa (i) .ne. -i) stop 203
    else
      if (aa (i) .ne. i) stop 204
    end if
  end do

end program

