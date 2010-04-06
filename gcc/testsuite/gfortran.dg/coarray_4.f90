! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Coarray support -- corank declarations
! PR fortran/18918
!

subroutine valid(n, c, f)
  implicit none
  integer :: n
  integer, save :: a[*], b(4)[-1:4,*]
  real :: c(*)[1,0:3,3:*]
  real :: f(n)[0:n,-100:*]
  integer, allocatable :: d[:], e(:)[:,:]
  integer, save, codimension[1,*] :: g, h(7), i(6)[*], j[*]
  integer :: k
  codimension :: k[*]
  save :: k
  integer :: ii = 7
  block
    integer, save :: kk[ii, *] ! { dg-error "cannot have the SAVE attribute" }
  end block
end subroutine valid

subroutine valid2()
  type t
    integer, allocatable :: a[:]
  end type t
  type, extends(t) :: tt
    integer, allocatable :: b[:]
  end type tt
  type(tt), save :: foo
  type(tt) :: bar ! { dg-error "is a coarray or has a coarray component" }
end subroutine valid2

subroutine invalid(n)
  implicit none
  integer :: n
  integer :: k[*] ! { dg-error "not ALLOCATABLE, SAVE nor a dummy" }
  integer :: h(3)[*] ! { dg-error "not ALLOCATABLE, SAVE nor a dummy" }
  integer, save :: a[*]
  codimension :: a[1,*] ! { dg-error "Duplicate CODIMENSION attribute" }
  complex, save :: hh(n)[*] ! { dg-error "cannot have the SAVE attribute" }
  integer :: j = 6

  integer, save :: hf1[j,*] ! { dg-error "cannot appear in the expression|cannot have the SAVE attribute" }
  integer, save :: hf2[n,*] ! { dg-error "cannot have the SAVE attribute" }
  integer, save :: hf3(4)[j,*] ! { dg-error "cannot appear in the expression|cannot have the SAVE attribute" }
  integer, save :: hf4(5)[n,*] ! { dg-error "cannot have the SAVE attribute" }

  integer, allocatable :: a2[*] ! { dg-error "must have deferred shape" }
  integer, allocatable :: a3(:)[*] ! { dg-error "must have deferred shape" }
  integer, allocatable :: a4[*] ! { dg-error "must have deferred shape" }
end subroutine invalid

subroutine invalid2
  use iso_c_binding
  implicit none
  type t0
    integer, allocatable :: a[:,:,:]
  end type t0
  type t
  end type t
  type, extends(t) :: tt ! { dg-error "has a coarray component, parent type" }
    integer, allocatable :: a[:]
  end type tt
  type ttt
    integer, pointer :: a[:] ! { dg-error "must be allocatable" }
  end type ttt
  type t4
    integer, allocatable :: b[4,*] ! { dg-error "with deferred shape" }
  end type t4
  type t5
    type(c_ptr), allocatable :: p[:] ! { dg-error "shall not be a coarray" }
  end type t5
  type(t0), save :: t0_1[*] ! { dg-error "shall be a nonpointer, nonallocatable scalar" }
  type(t0), allocatable :: t0_2[:] ! { dg-error "shall be a nonpointer, nonallocatable scalar" }
  type(c_ptr), save :: pp[*] ! { dg-error "shall not be a coarray" }
end subroutine invalid2

elemental subroutine elem(a) ! { dg-error "Coarray dummy argument" }
  integer, intent(in) :: a[*]
end subroutine

function func() result(res)
  integer :: res[*] ! { dg-error "CODIMENSION attribute conflicts with RESULT" }
end function func
