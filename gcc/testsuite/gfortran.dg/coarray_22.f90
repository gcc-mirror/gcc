! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Constraint checks for invalid access of remote pointers
! (Accessing the value is ok, checking/changing association
!  status is invalid)
!
! PR fortran/18918
!
type t
  integer, pointer :: ptr => null()
end type t
type(t) :: x[*], y[*]

if (associated(x%ptr)) stop 0
if (associated(x%ptr,y%ptr)) stop 0

if (associated(x[1]%ptr)) stop 0  ! { dg-error "shall not be conindexed" }
if (associated(x%ptr,y[1]%ptr)) stop 0  ! { dg-error "shall not be conindexed" }

nullify (x%ptr)
nullify (x[1]%ptr)  ! { dg-error "shall not be conindexed" }

x%ptr => null(x%ptr)
x%ptr => null(x[1]%ptr)  ! { dg-error "shall not be conindexed" }
x[1]%ptr => null(x%ptr)  ! { dg-error "shall not have a coindex" }

allocate(x%ptr)
deallocate(x%ptr)

allocate(x[1]%ptr)  ! { dg-error "Coindexed allocatable object" }
deallocate(x[1]%ptr)  ! { dg-error "Coindexed allocatable object" }
end
