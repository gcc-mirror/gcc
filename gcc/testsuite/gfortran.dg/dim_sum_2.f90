! { dg-do  run }
! { dg-shouldfail "Dim argument incorrect in SUM intrinsic: is 5, should be between 1 and 2" }

program summation

  integer, parameter :: n1=5, n2=7
  integer, dimension(1:n1,1:n2) :: arr
  integer, dimension(1:n1) :: r2
  integer, dimension(1:n2) :: r1
  integer :: i,j
  character(len=80) :: c1, c2
  character(len=50) :: fmt = '(10I5)'
  do j=1,n2
     do i=1,n1
        arr(i,j) = i+j*10
     end do
  end do
  
  r1 = sum(arr,dim=1,mask=arr>23)
  write (unit=c2, fmt=fmt) r1
  call print_sum(1,c1)
  if (c1 /= c2) call abort
  r2 = sum(arr,dim=2,mask=arr>23)
  write (unit=c2, fmt=fmt) r2
  call print_sum(2,c1)
  if (c1 /= c2) call abort
  call print_sum(5,c1)

contains

  subroutine print_sum(d, c)
    integer, intent(in) :: d
    character(len=80), intent(out) :: c
    write (unit=c, fmt=fmt) sum(arr,dim=d,mask=arr>23)
   end subroutine

end
