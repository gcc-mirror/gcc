! { dg-do compile }
! { dg-additional-options "-fcoarray=single" }
!
! PR fortran/106856
!
!
!
subroutine foo(x,y)
  class(*), optional :: x, y
  optional    :: x    ! { dg-error "Duplicate OPTIONAL attribute" }
  target      :: x
  allocatable :: x
  target      :: x    ! { dg-error "Duplicate TARGET attribute" }
  allocatable :: x    ! { dg-error "Duplicate ALLOCATABLE attribute" }
  pointer     :: y
  contiguous  :: y
  pointer     :: y    ! { dg-error "Duplicate POINTER attribute" }
  contiguous  :: y    ! { dg-error "Duplicate CONTIGUOUS attribute" }
  codimension :: x[:]
  dimension   :: x(:,:)
  dimension   :: y(:,:,:)
  codimension :: x[:] ! { dg-error "Duplicate CODIMENSION attribute" }
  dimension   :: y(:) ! { dg-error "Duplicate DIMENSION attribute" }
end
