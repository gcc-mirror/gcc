! PR fortran/62131
! { dg-do compile }
! { dg-options "-fopenmp" }

program pr62131
  integer,allocatable :: nerrs(:,:)
  allocate(nerrs(10,10))
  nerrs(:,:) = 0
!$omp parallel do
  do k=1,10
    call uperrs(k,1)
  end do
contains
  subroutine uperrs(i,io)
    integer,intent(in) :: i,io
!$omp atomic
    nerrs(i,io)=nerrs(i,io)+1
  end subroutine
end
