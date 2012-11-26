! { dg-do compile }
! PR fortran/55314 - the second allocate statement was rejected.

program main
  implicit none
  integer :: max_nb
  type comm_mask
    integer(4), pointer :: mask(:)
  end type comm_mask
  type (comm_mask), allocatable, save :: encode(:,:)
  max_nb=2
  allocate( encode(1:1,1:max_nb))
  allocate( encode(1,1)%mask(1),encode(1,2)%mask(1))
  deallocate( encode(1,1)%mask,encode(1,2)%mask)
  allocate( encode(1,1)%mask(1),encode(1,1)%mask(1))  ! { dg-error "also appears at" }
end program main
