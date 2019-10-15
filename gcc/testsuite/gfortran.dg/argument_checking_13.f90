! { dg-do compile }
!
! PR fortran/34796
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
subroutine test1(assumed_sh_dummy, pointer_dummy)
implicit none
interface
  subroutine rlv1(y)
    real   :: y(3)
  end subroutine rlv1
end interface

real          :: assumed_sh_dummy(:,:,:)
real, pointer :: pointer_dummy(:,:,:)

real, allocatable :: deferred(:,:,:)
real, pointer     :: ptr(:,:,:)
call rlv1(deferred(1,1,1))         ! valid since contiguous
call rlv1(ptr(1,1,1))              ! { dg-error "Element of assumed-shape or pointer array" }
call rlv1(assumed_sh_dummy(1,1,1)) ! { dg-error "Element of assumed-shape or pointer array" }
call rlv1(pointer_dummy(1,1,1))    ! { dg-error "Element of assumed-shape or pointer array" }
end

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
call rlv2(ptr(1,1,1))              ! Valid F2003
call rlv2(assumed_sh_dummy(1,1,1)) ! Valid F2003
call rlv2(pointer_dummy(1,1,1))    ! Valid F2003

! The following is kind of ok: The memory access it valid
! We warn nonetheless as the result is not what is intented
! and also formally wrong.
! Using (1:string_length) would be ok.
call rlv2(ptr(1,1,1)(1:1))              ! { dg-error "contains too few elements" }
call rlv2(assumed_sh_dummy(1,1,1)(1:2)) ! { dg-error "contains too few elements" }
call rlv2(pointer_dummy(1,1,1)(1:3))    ! Valid F2003
end

subroutine test3(assumed_sh_dummy, pointer_dummy)
implicit none
interface
  subroutine rlv3(y)
    character   :: y(3)
  end subroutine rlv3
end interface

character(2)          :: assumed_sh_dummy(:,:,:)
character(2), pointer :: pointer_dummy(:,:,:)

character(2), allocatable :: deferred(:,:,:)
character(2), pointer     :: ptr(:,:,:)
call rlv3(deferred(1,1,1))         ! Valid since contiguous
call rlv3(ptr(1,1,1))              ! { dg-error "contains too few elements" }
call rlv3(assumed_sh_dummy(1,1,1)) ! { dg-error "contains too few elements" }
call rlv3(pointer_dummy(1,1,1))    ! { dg-error "contains too few elements" }

call rlv3(deferred(1,1,1)(1:2))         ! Valid since contiguous
call rlv3(ptr(1,1,1)(1:2))              ! { dg-error "contains too few elements" }
call rlv3(assumed_sh_dummy(1,1,1)(1:2)) ! { dg-error "contains too few elements" }
call rlv3(pointer_dummy(1,1,1)(1:2))    ! { dg-error "contains too few elements" }
end
