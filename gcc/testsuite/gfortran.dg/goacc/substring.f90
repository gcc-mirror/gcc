implicit none
character(len=10) :: str1, str2(5,5)

type t
  character(len=10) :: str1, str2(5,5)
end type t
type(t) :: v

!$acc enter data copyin(v%str1)       ! OK
!$acc enter data copyin(v%str2)       ! OK
!$acc enter data copyin(v%str2(1,2))  ! OK
!$acc enter data copyin(str1)         ! OK
!$acc enter data copyin(str2)         ! OK
!$acc enter data copyin(str2(1,2))    ! OK

!$acc enter data copyin(v%str1(2:5))       ! { dg-error "Unexpected substring reference in MAP clause" }
!$acc enter data copyin(v%str2(1,2)(2:4))  ! { dg-error "Unexpected substring reference in MAP clause" }
!$acc enter data copyin(str1(2:5))         ! { dg-error "Unexpected substring reference in MAP clause" }
!$acc enter data copyin(str2(1,2)(2:4))    ! { dg-error "Unexpected substring reference in MAP clause" }

!$acc parallel
!$acc update host(v%str1(2:5))             ! { dg-error "Unexpected substring reference in MAP clause" }
!$acc update host(v%str2(1,2)(2:4))        ! { dg-error "Unexpected substring reference in MAP clause" }
!$acc update host(str1(2:5))               ! { dg-error "Unexpected substring reference in MAP clause" }
!$acc update host(str2(1,2)(2:4))          ! { dg-error "Unexpected substring reference in MAP clause" }
!$acc end parallel
end
