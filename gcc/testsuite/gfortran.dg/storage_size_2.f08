! { dg-do compile }
!
! PR 44649: [OOP] F2008: storage_size intrinsic
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

use iso_c_binding, only: c_int, c_sizeof

type, bind(c) :: t 
  integer(c_int) :: j
end type

integer(4) :: i1
integer(c_int) :: i2
type(t) :: x

print *,c_sizeof(i1)                ! { dg-error "must be be an interoperable data entity" }
print *,c_sizeof(i2)
print *,c_sizeof(x)
print *, c_sizeof(ran())            ! { dg-error "must be be an interoperable data entity" }

print *,storage_size(1.0,4)
print *,storage_size(1.0,3.2)       ! { dg-error "must be INTEGER" }
print *,storage_size(1.0,(/1,2/))   ! { dg-error "must be a scalar" }
print *,storage_size(1.0,irand())   ! { dg-error "must be a constant" }

end
