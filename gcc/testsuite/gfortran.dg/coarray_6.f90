! { dg-do compile }
!
! Coarray support -- corank declarations
! PR fortran/18918
!
module m2
  use iso_c_binding
  integer(c_int), bind(C) :: a[*] ! { dg-error "BIND.C. attribute conflicts with CODIMENSION" }

  type, bind(C) :: t ! { dg-error "cannot have the ALLOCATABLE" }
    integer(c_int), allocatable :: a[:] ! { dg-error "cannot have the ALLOCATABLE" }
    integer(c_int)  :: b[*] ! { dg-error "must be allocatable" }
  end type t
end module m2

subroutine bind(a) bind(C) ! { dg-error "Coarray dummy variable" }
  use iso_c_binding
  integer(c_int) :: a[*]
end subroutine bind

subroutine allo(x) ! { dg-error "can thus not be an allocatable coarray" }
  integer, allocatable, intent(out) :: x[:]
end subroutine allo

module m
  integer :: modvar[*] ! OK, implicit save
  type t
    complex, allocatable :: b(:,:,:,:)[:,:,:]
  end type t
end module m

subroutine bar()
  integer, parameter :: a[*] = 4 ! { dg-error "PARAMETER attribute conflicts with CODIMENSION" }
  integer, pointer :: b[:] ! { dg-error "is not ALLOCATABLE, SAVE nor a dummy" }
end subroutine bar

subroutine vol()
  integer,save :: a[*]
  block
    volatile :: a ! { dg-error "Specifying VOLATILE for coarray" }
  end block
contains
  subroutine int()
    volatile :: a ! { dg-error "Specifying VOLATILE for coarray" }
  end subroutine int
end subroutine vol


function func() result(func2) ! { dg-error "shall not be a coarray or have a coarray component" }
  use m
  type(t) :: func2
end function func

program main
  integer :: A[*] ! Valid, implicit SAVE attribute
end program main

! { dg-final { cleanup-modules "m" } }
