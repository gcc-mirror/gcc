! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
use iso_fortran_env
implicit none

type t
  integer, pointer :: caf2[:] ! { dg-error "must be allocatable with deferred shape" }
end type t

integer, pointer :: caf[*] ! { dg-error "POINTER attribute conflicts with CODIMENSION attribute" }

type t2
  type(lock_type), pointer :: lock_it ! { dg-error "Component lock_it at .1. of type LOCK_TYPE must have a codimension or be a subcomponent of a coarray, which is not possible as the component has the pointer attribute" }
end type t2
type(t2) :: caf3[*]

type t3
  type(lock_type) :: x
end type t3

type t4
  type(t3), pointer :: y ! { dg-error "Pointer component y at .1. has a noncoarray subcomponent of type LOCK_TYPE, which must have a codimension or be a subcomponent of a coarray" }
end type t4

end
