! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Two simple diagnostics, which were initially not thought of
!
! General coarray PR: PR fortran/18918
! 

subroutine one
    integer, allocatable :: a(:)[:,:]  ! corank = 2
    integer :: index,nn1,nn2,nn3,mm0

    allocate(a(mm0)[nn1:nn2,nn3,*]) ! { dg-error "Too many codimensions at .1., expected 2 not 3" }
end subroutine one

subroutine two
    integer, allocatable :: a(:)[:,:,:], b(:)[:,:], c(:)[:]
    index1 = image_index(a, [2, 1, 1] )  !OK
    index2 = image_index(b, [2, 1, 1] )  ! { dg-error "array elements of the SUB argument to IMAGE_INDEX at .1. shall be 2 .corank. not 3" }
    index3 = image_index(c, [1] )        !OK
end subroutine two
