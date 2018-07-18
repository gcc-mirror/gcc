! { dg-do  run }
! Take cshift through its paces to make sure no boundary
! cases are wrong.

module kinds
  integer, parameter :: sp = selected_real_kind(6) ! Single precision
end module kinds

module replacements
  use kinds
contains
  subroutine cshift_sp_3_v1 (array, shift, dim, res)
    integer, parameter :: wp = sp
    real(kind=wp), dimension(:,:,:), intent(in) :: array
    integer, intent(in) :: shift, dim
    real(kind=wp), dimension(:,:,:), intent(out) :: res
    integer :: i,j,k
    integer :: sh, rsh
    integer :: n
    integer :: n2, n3
    res = 0
    n3 = size(array,3)
    n2 = size(array,2)
    n1 = size(array,1)
    if (dim == 1) then
       n = n1
       sh = modulo(shift, n)
       rsh = n - sh
       do k=1, n3
          do j=1, n2
             do i=1, rsh
                res(i,j,k) = array(i+sh,j,k)
             end do
             do i=rsh+1,n
                res(i,j,k) = array(i-rsh,j,k)
             end do
          end do
       end do
    else if (dim == 2) then
       n = n2
       sh = modulo(shift,n)
       rsh = n - sh
       do k=1, n3
          do j=1, rsh
             do i=1, n1
                res(i,j,k) = array(i,j+sh, k)
             end do
          end do
          do j=rsh+1, n
             do i=1, n1
                res(i,j,k) = array(i,j-rsh, k)
             end do
          end do
       end do
    else if (dim == 3) then
       n = n3
       sh = modulo(shift, n)
       rsh = n - sh
       do k=1, rsh
          do j=1, n2
             do i=1, n1
                res(i,j,k) = array(i, j, k+sh)
             end do
          end do
       end do
       do k=rsh+1, n
          do j=1, n2
             do i=1, n1
                res(i,j, k) = array(i, j, k-rsh)
             end do
          end do          
       end do
    else
       stop "Wrong argument to dim"
    end if
  end subroutine cshift_sp_3_v1
end module replacements

program testme
  use kinds
  use replacements
  implicit none
  integer, parameter :: wp = sp  ! Working precision
  INTEGER, PARAMETER :: n = 7
  real(kind=wp), dimension(:,:,:), allocatable :: a,b,c
  integer i, j, k
  real:: t1, t2
  integer, parameter :: nrep = 20

  allocate (a(n,n,n), b(n,n,n),c(n,n,n))
  call random_number(a)
  do k = 1,3
   do i=-3,3,2
     call cshift_sp_3_v1 (a, i, k, b)
     c = cshift(a,i,k)
     if (any (c /= b)) STOP 1
   end do
  end do
  deallocate (b,c)
  allocate (b(n-1,n-1,n-1),c(n-1,n-1,n-1))
  do k=1,3
     do i=-3,3,2
        call cshift_sp_3_v1 (a(1:n-1,1:n-1,1:n-1), i, k, b)
        c = cshift(a(1:n-1,1:n-1,1:n-1), i, k)
        if (any (c /= b)) STOP 2
     end do
  end do
end program testme
