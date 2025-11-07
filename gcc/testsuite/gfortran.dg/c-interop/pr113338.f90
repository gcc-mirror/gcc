! { dg-do run }
! { dg-additional-sources pr113338-c.c }
! { dg-additional-options "-Wno-error -O2 -std=f2018" }
! { dg-warning "command-line option '-std=f2018' is valid for Fortran but not for C" "" { target *-*-* } 0 }
!
! PR fortran/113338 - F2018 extensions to interoperability of procedures

program example
  use iso_c_binding
  implicit none

  type :: t
     integer :: i
  end type

  interface
     subroutine c_proc(x) bind(c)
       import t
       type(t), pointer, intent(in) :: x
     end subroutine c_proc
  end interface

  type(t), target :: x

  x%i = 42
  call c_proc(x)
end program

! pointer
subroutine f_proc(x) bind(c)
  type :: t
     integer :: i
  end type t
  type(t), pointer, intent(in) :: x
  if (.not. associated (x)) stop 1
! print *, x%i
  if (x%i /= 42) stop 2
end subroutine f_proc

!-----------------------------------------------------------------------
! Further cases some of which are also tested elsewhere in the testsuite
!-----------------------------------------------------------------------

! character: length 1 or assumed character length -> *CFI_cdesc_t
subroutine f_char(c, s) bind(c)
  character    :: c(:)
  character(*) :: s(:)
end subroutine f_char

! allocatable: scalar, assumed-shape, assumed-rank -> *CFI_cdesc_t
subroutine f_a(x, y, z) bind(c)
  type :: t
     integer :: i
  end type t
  type(t), allocatable :: x
  type(t), allocatable :: y(:)
  type(t), allocatable :: z(..)
end subroutine f_a

! pointer: scalar, assumed-shape, assumed-rank -> *CFI_cdesc_t
subroutine f_p(x, y, z) bind(c)
  type :: t
     integer :: i
  end type t
  type(t), pointer :: x
  type(t), pointer :: y(:)
  type(t), pointer :: z(..)
end subroutine f_p

! assumed-type: assumed shape, assumed rank -> *CFI_cdesc_t
subroutine f_at_cfi(z, w) bind(c)
  type(*) :: z(:)
  type(*) :: w(..)
end subroutine f_at_cfi

! assumed-type: scalar, assumed-size -> *void
subroutine f_at_void(x, y) bind(c)
  type(*) :: x
  type(*) :: y(*)
end subroutine f_at_void
