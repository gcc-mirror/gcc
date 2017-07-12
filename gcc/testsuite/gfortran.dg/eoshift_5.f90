! { dg-do  run }
! Check that eoshift works for three-dimensional arrays.
module x
  implicit none
contains
  subroutine eoshift_1 (array, shift, boundary, dim, res)
    real, dimension(:,:,:), intent(in) :: array
    real, dimension(:,:,:), intent(out) :: res
    integer, dimension(:,:), intent(in) :: shift
    real, optional, intent(in) :: boundary
    integer, optional, intent(in) :: dim
    integer :: s1, s2, s3
    integer :: n1, n2, n3
    integer :: sh
    real :: b
    integer :: d

    if (present(boundary)) then
       b = boundary
    else
       b = 0.0
    end if

    if (present(dim)) then
       d = dim
    else
       d = 1
    end if

    n1 = size(array,1)
    n2 = size(array,2)
    n3 = size(array,3)

    select case(dim)
    case(1)
       do s3=1,n3
          do s2=1,n2
             sh = shift(s2,s3)
             if (sh > 0) then
                sh = min(sh, n1)
                do s1= 1, n1 - sh
                   res(s1,s2,s3) = array(s1+sh,s2,s3)
                end do
                do s1 = n1 - sh + 1,n1
                   res(s1,s2,s3) = b
                end do
             else
                sh = max(sh, -n1)
                do s1=1,-sh
                   res(s1,s2,s3) = b
                end do
                do s1= 1-sh,n1
                   res(s1,s2,s3) = array(s1+sh,s2,s3)
                end do
             end if
          end do
       end do
    case(2)
       do s3=1,n3
          do s1=1,n1
             sh = shift(s1,s3)
             if (sh > 0) then
                sh = min (sh, n2)
                do s2=1, n2 - sh
                   res(s1,s2,s3) = array(s1,s2+sh,s3)
                end do
                do s2=n2 - sh + 1, n2
                   res(s1,s2,s3) = b
                end do
             else
                sh = max(sh, -n2)
                do s2=1,-sh
                   res(s1,s2,s3) = b
                end do
                do s2=1-sh,n2
                   res(s1,s2,s3) = array(s1,s2+sh,s3)
                end do
             end if
          end do
       end do

    case(3)
       do s2=1, n2
          do s1=1,n1
             sh = shift(s1, s2)
             if (sh > 0) then
                sh = min(sh, n3)
                do s3=1,n3 - sh
                   res(s1,s2,s3) = array(s1,s2,s3+sh)
                end do
                do s3=n3 - sh + 1, n3
                   res(s1,s2,s3) = b
                end do
             else
                sh = max(sh, -n3)
                do s3=1,-sh
                   res(s1,s2,s3) = b
                end do
                do s3=1-sh,n3
                   res(s1,s2,s3) = array(s1,s2,s3+sh)
                end do
             end if
          end do
       end do
       
    case default
       stop "Illegal dim"
    end select
  end subroutine eoshift_1
  subroutine fill_shift(x, n)
    integer, intent(out), dimension(:,:) :: x
    integer, intent(in) :: n
    integer :: n1, n2, s1, s2
    integer :: v
    v = -n - 1
    n1 = size(x,1)
    n2 = size(x,2)
    do s2=1,n2
       do s1=1,n1
          x(s1,s2) = v
          v = v + 1
          if (v > n + 1) v = -n - 1
       end do
    end do
  end subroutine fill_shift
end module x

program main
  use x
  implicit none
  integer, parameter :: n1=20,n2=30,n3=40
  real, dimension(n1,n2,n3) :: a,b,c
  real, dimension(2*n1,n2,n3) :: a2, c2
  integer :: dim
  integer, dimension(n2,n3), target :: sh1
  integer, dimension(n1,n3), target :: sh2
  integer, dimension(n1,n2), target :: sh3
  real, dimension(n2,n3), target :: b1
  real, dimension(n1,n3), target :: b2
  real, dimension(n1,n2), target :: b3

  integer, dimension(:,:), pointer :: sp
  real, dimension(:,:), pointer :: bp

  call random_number(a)
  call fill_shift(sh1, n1)
  call fill_shift(sh2, n2)
  call fill_shift(sh3, n3)

  do dim=1,3
     if (dim == 1) then
        sp => sh1
     else if (dim == 2) then
        sp => sh2
     else
        sp => sh3
     end if
     b = eoshift(a,shift=sp,dim=dim,boundary=-0.5)
     call eoshift_1 (a, shift=sp, dim=dim, boundary=-0.5,res=c)
     if (any (b /= c)) then
        print *,"dim = ", dim
        print *,"sp = ", sp
        print '(99F8.4)',b
        print '(99F8.4)',c
        call abort
     end if
     a2 = 42.
     a2(1:2*n1:2,:,:) = a
     b = eoshift(a2(1:2*n1:2,:,:), shift=sp, dim=dim, boundary=-0.5)
     if (any(b /= c)) then
        call abort
     end if
     c2 = 43.
     c2(1:2*n1:2,:,:) = eoshift(a, shift=sp, dim=dim, boundary=-0.5)
     if (any(c2(1:2*n1:2,:,:) /= c)) then
        call abort
     end if
     if (any(c2(2:2*n1:2,:,:) /= 43.)) then
        call abort
     end if
  end do
end program main
