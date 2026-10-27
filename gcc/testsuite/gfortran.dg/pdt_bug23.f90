! { dg-do run }
! ALLOCATE with MOLD= did not propagate the LEN parameter; SOURCE= does.
! The abreviated form of MODULE PROCEDURE was found not to work and has been fixed.
! See attachment to PR82173 comment 10 - ~/reducers/bug23-submodule-pdt-result.f90
module s_m
  type :: t(n)
    integer, len :: n = 1
    real :: v(n)
  end type
  interface
! First test the MODULE FUNCTIONs
    module function make (arg1) result (r)  ! Explicit result using integer arg
      integer, intent(in) :: arg1
      type(t(arg1)) :: r
    end function
    module function make2 (arg1) result (r) ! Explicit result using deferred len arg
      type(t(:)), allocatable :: arg1
      type(t(arg1%n)) :: r
    end function
    module function make3 (arg1) result (r) ! Explicit result using assumed len arg
      type(t(*)) :: arg1
      type(t(arg1%n)) :: r
    end function
! Then the MODULE PROCEDUREs
    module function make4 (arg1) result (r) ! Explicit result using integer arg
      integer, intent(in) :: arg1
      type(t(arg1)) :: r
    end function
    module function make5 (arg1) result (r) ! Explicit result using assumed len arg
      type(t(*)), intent(in) :: arg1
      type(t(arg1%n)) :: r
    end function
    module function make6 (arg1) result (r) ! Explicit result using deferred len arg
      type(t(:)), allocatable, intent(in) :: arg1
      type(t(arg1%n)) :: r
    end function
  end interface
end module

submodule (s_m) s_impl
contains
! test module functions
  module function make (arg1) result (r)
    integer, intent(in) :: arg1
    type(t(arg1)) :: r
    r%v = 2.0
  end function
  module function make2 (arg1) result (r)
    type(t(:)), allocatable :: arg1
    type(t(arg1%n)) :: r
    if (allocated (arg1)) then
      r = arg1
    else
      allocate (arg1, source = t(5)([1.,2.,3.,4.,5.]))
      r = arg1
    end if
  end function
  module function make3 (arg1) result (r)
    type(t(*)) :: arg1
    type(t(arg1%n)) :: r
    r%v = arg1%v - 5
  end function

! Test module procedures
  module procedure make4
    integer :: i
    r%v = [(real (i), i = 1, size (r%v, 1))]
  end procedure
  module procedure make5
    r%v = 3.0 * arg1%v
  end procedure
  module procedure make6
    if (.not.allocated (arg1)) stop 99
    r%v = arg1%v / 3.0
  end procedure
end submodule

program v3
  use s_m
  type(t(5)) :: a
  type(t(:)), allocatable :: b
  integer :: i
  a = make (5)
  IF (a%n /= 5) STOP 1
  IF (INT (sum (a%v)) /= 10) STOP 2
  allocate (b, source = t(5)([1.,2.,3.,4.,5.]))
  a = make2 (b)
  if (int (sum (a%v)) /= 15) stop 3
  b = make3 (a)
  if (int (sum (b%v)) /= -10) stop 4
  b = make4 (10)
  if (any (int (b%v) /= [(i, i = 1, b%n)])) stop 5
  b = make5 (a)
  if (any (int (b%v) /= 3 * int (a%v))) stop 6
  b = make6 (b)
  if (int (sum (b%v)) /= 15) stop 7
  deallocate (b)
end program
