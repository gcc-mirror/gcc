! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/34796
!
! This checks for Fortran 2003 extensions.
!
! Argument checks:
! - elements of deferred-shape arrays (= non-dummies) are allowed
!   as the memory is contiguous
! - while assumed-shape arrays (= dummy arguments) and pointers are
!   not (strides can make them non-contiguous)
! and
! - if the memory is non-contigous, character arguments have as
!   storage size only the size of the element itself, check for
!   too short actual arguments.
!
subroutine test2(assumed_sh_dummy, pointer_dummy)
implicit none
interface
  subroutine rlv2(y)
    character   :: y(3)
  end subroutine rlv2
end interface

character(3)          :: assumed_sh_dummy(:,:,:)
character(3), pointer :: pointer_dummy(:,:,:)

character(3), allocatable :: deferred(:,:,:)
character(3), pointer     :: ptr(:,:,:)
call rlv2(deferred(1,1,1))         ! Valid since contiguous
call rlv2(ptr(1,1,1))              ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv2(assumed_sh_dummy(1,1,1)) ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv2(pointer_dummy(1,1,1))    ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }

! The following is kind of ok: The memory access it valid
! We warn nonetheless as the result is not what is intented
! and also formally wrong.
! Using (1:string_length) would be ok.
call rlv2(deferred(1,1,1)(1:3))         ! OK
call rlv2(ptr(1,1,1)(1:1))              ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv2(assumed_sh_dummy(1,1,1)(1:2)) ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv2(pointer_dummy(1,1,1)(1:3))    ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
end

subroutine test3(assumed_sh_dummy, pointer_dummy)
implicit none
interface
  subroutine rlv3(y)
    character   :: y(2)
  end subroutine rlv3
end interface

character(2)          :: assumed_sh_dummy(:,:,:)
character(2), pointer :: pointer_dummy(:,:,:)

character(2), allocatable :: deferred(:,:,:)
character(2), pointer     :: ptr(:,:,:)
call rlv3(deferred(1,1,1))         ! Valid since contiguous
call rlv3(ptr(1,1,1))              ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv3(assumed_sh_dummy(1,1,1)) ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv3(pointer_dummy(1,1,1))    ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }

call rlv3(deferred(1,1,1)(1:2))         ! Valid since contiguous
call rlv3(ptr(1,1,1)(1:2))              ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv3(assumed_sh_dummy(1,1,1)(1:2)) ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
call rlv3(pointer_dummy(1,1,1)(1:2))    ! { dg-error "Fortran 2003: Scalar CHARACTER actual" }
end
