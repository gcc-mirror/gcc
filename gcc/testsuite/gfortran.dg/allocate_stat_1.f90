! { dg-do run }
! Check whether uppon failure no run-time error is issued.
! PR fortran/32124
!
program mem
  implicit none
  real(8), allocatable :: A(:,:,:,:)
  integer :: status

  status = 0
  allocate(A(huge(0),huge(0),huge(0),huge(0)),stat=status) ! this should fail
  if(status == 0) call abort()

  status = 0
  deallocate(A,stat=status)
  if(status == 0) call abort()
end program mem
