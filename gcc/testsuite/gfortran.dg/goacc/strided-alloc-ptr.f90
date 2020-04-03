implicit none
type t
  integer, allocatable :: i, j(:)
  integer, pointer :: k, ll(:)
end type t
type(t) :: x(2)

!$acc enter data copyin(x)

!$acc enter data copyin(x(:)%i)
! { dg-error "Component to the right of a part reference with nonzero rank must not have the ALLOCATABLE attribute" "" { target "*-*-*" } 10 }
! { dg-error ".x. in MAP clause at .1. is not a proper array section"  "" { target "*-*-*" } 10 }

!$acc enter data copyin(x(:)%j(3))
! { dg-error "Component to the right of a part reference with nonzero rank must not have the ALLOCATABLE attribute" "" { target "*-*-*" } 14 }
! { dg-error ".x. in MAP clause at .1. is not a proper array section"  "" { target "*-*-*" } 14 }

!$acc enter data copyin(x(:)%j)
! { dg-error "Component to the right of a part reference with nonzero rank must not have the ALLOCATABLE attribute" "" { target "*-*-*" } 18 }
! { dg-error ".x. in MAP clause at .1. is not a proper array section"  "" { target "*-*-*" } 18 }


!$acc enter data copyin(x(:)%k)
! { dg-error "Component to the right of a part reference with nonzero rank must not have the POINTER attribute" "" { target "*-*-*" } 23 }
! { dg-error ".x. in MAP clause at .1. is not a proper array section"  "" { target "*-*-*" } 23 }

!$acc enter data copyin(x(:)%ll(3))
! { dg-error "Component to the right of a part reference with nonzero rank must not have the POINTER attribute" "" { target "*-*-*" } 27 }
! { dg-error ".x. in MAP clause at .1. is not a proper array section"  "" { target "*-*-*" } 27 }

!$acc enter data copyin(x(:)%ll)
! { dg-error "Component to the right of a part reference with nonzero rank must not have the POINTER attribute" "" { target "*-*-*" } 31 }
! { dg-error ".x. in MAP clause at .1. is not a proper array section"  "" { target "*-*-*" } 31 }
end
