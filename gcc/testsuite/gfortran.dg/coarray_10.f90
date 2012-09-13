! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/18918
!
! Coarray intrinsics
!

subroutine image_idx_test1()
  INTEGER,save :: array[2,-1:4,8,*]
  WRITE (*,*) IMAGE_INDEX (array, [2,0,3,1])
  WRITE (*,*) IMAGE_INDEX (array, [0,0,3,1])  ! { dg-error "for dimension 1, SUB has 0 and COARRAY lower bound is 1" }
  WRITE (*,*) IMAGE_INDEX (array, [1,2,9,0])  ! { dg-error "for dimension 3, SUB has 9 and COARRAY upper bound is 8" }
  WRITE (*,*) IMAGE_INDEX (array, [2,0,3])    ! { dg-error "Too few elements" }
  WRITE (*,*) IMAGE_INDEX (array, [2,0,3,1,1])! { dg-error "Too many elements" }
end subroutine

subroutine this_image_check()
  integer,save :: a(1,2,3,5)[0:3,*]
  integer :: j
  integer,save :: z(4)[*], i

  j = this_image(a,dim=3) ! { dg-error "not a valid codimension index" }
  j = this_image(dim=3) ! { dg-error "DIM argument without ARRAY argument" }
  i = image_index(i, [ 1 ]) ! { dg-error "Expected coarray variable" }
  i = image_index(z, 2) ! { dg-error "must be a rank one array" }
end subroutine this_image_check


subroutine rank_mismatch()
  implicit none
  integer,allocatable :: A(:)[:,:,:,:]
  allocate(A(1)[1,1,1:*])     ! { dg-error "Too few codimensions" }
  allocate(A(1)[1,1,1,1,1,*]) ! { dg-error "Invalid codimension 5" }
  allocate(A(1)[1,1,1,*])
  allocate(A(1)[1,1])     ! { dg-error "Too few codimensions" }
  allocate(A(1)[1,*])     ! { dg-error "Too few codimensions" }
  allocate(A(1)[1,1:*])   ! { dg-error "Too few codimensions" }

  A(1)[1,1,1] = 1       ! { dg-error "Too few codimensions" }
  A(1)[1,1,1,1,1,1] = 1 ! { dg-error "Invalid codimension 5" }
  A(1)[1,1,1,1] = 1
  A(1)[1,1] = 1         ! { dg-error "Too few codimensions" }
  A(1)[1,1] = 1         ! { dg-error "Too few codimensions" }
  A(1)[1,1:1] = 1       ! { dg-error "Too few codimensions" }
end subroutine rank_mismatch
