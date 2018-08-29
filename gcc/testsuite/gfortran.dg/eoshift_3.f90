! { dg-do  run }
! Check that eoshift works for three-dimensional arrays.
module x
  implicit none
contains
  subroutine eoshift_0 (array, shift, boundary, dim, res)
    real, dimension(:,:,:), intent(in) :: array
    real, dimension(:,:,:), intent(out) :: res
    integer, value :: shift
    real, optional, intent(in) :: boundary
    integer, optional, intent(in) :: dim
    integer :: s1, s2, s3
    integer :: n1, n2, n3

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
       if (shift > 0) then
          shift = min(shift, n1)
          do s3=1,n3
             do s2=1,n2
                do s1= 1, n1 - shift
                   res(s1,s2,s3) = array(s1+shift,s2,s3)
                end do
                do s1 = n1 - shift + 1,n1
                   res(s1,s2,s3) = b
                end do
             end do
          end do

       else
          shift = max(shift, -n1)
          do s3=1,n3
             do s2=1,n2
                do s1=1,-shift
                   res(s1,s2,s3) = b
                end do
                do s1= 1-shift,n1
                   res(s1,s2,s3) = array(s1+shift,s2,s3)
                end do
             end do
          end do
       end if

    case(2)
       if (shift > 0) then
          shift = min(shift, n2)
          do s3=1,n3
             do s2=1, n2 - shift
                do s1=1,n1
                   res(s1,s2,s3) = array(s1,s2+shift,s3)
                end do
             end do
             do s2=n2 - shift + 1, n2
                do s1=1,n1
                   res(s1,s2,s3) = b
                end do
             end do
          end do
       else
          shift = max(shift, -n2)
          do s3=1,n3
             do s2=1,-shift
                do s1=1,n1
                   res(s1,s2,s3) = b
                end do
             end do
             do s2=1-shift,n2
                do s1=1,n1
                   res(s1,s2,s3) = array(s1,s2+shift,s3)
                end do
             end do
          end do
       end if

    case(3)
       if (shift > 0) then
          shift = min(shift, n3)
          do s3=1,n3 - shift
             do s2=1, n2
                do s1=1,n1
                   res(s1,s2,s3) = array(s1,s2,s3+shift)
                end do
             end do
          end do
          do s3=n3 - shift + 1, n3
             do s2=1, n2
                do s1=1,n1
                   res(s1,s2,s3) = b
                end do
             end do
          end do
       else
          shift = max(shift, -n3)
          do s3=1,-shift
             do s2=1,n2
                do s1=1,n1
                   res(s1,s2,s3) = b
                end do
             end do
          end do
          do s3=1-shift,n3
             do s2=1,n2
                do s1=1,n1
                   res(s1,s2,s3) = array(s1,s2,s3+shift)
                end do
             end do
          end do
       end if

    case default
       stop "Illegal dim"
    end select
  end subroutine eoshift_0
end module x

program main
  use x
  implicit none
  integer, parameter :: n1=2,n2=4,n3=2
  real, dimension(n1,n2,n3) :: a,b,c
  integer :: dim, shift, shift_lim
  call random_number(a)

  do dim=1,3
     if (dim == 1) then
        shift_lim = n1 + 1
     else if (dim == 2) then
        shift_lim = n2 + 1
     else
        shift_lim = n3 + 1
     end if
     do shift=-shift_lim, shift_lim
        b = eoshift(a,shift,dim=dim)
        call eoshift_0 (a, shift=shift, dim=dim, res=c)
        if (any (b /= c)) then
                print *,"dim = ", dim, "shift = ", shift
                STOP 1
        end if
     end do
  end do
  call random_number(b)
  c = b

  do dim=1,3
     if (dim == 1) then
        shift_lim = n1/2 + 1
     else if (dim == 2) then
        shift_lim = n2/2 + 1
     else
        shift_lim = n3/2 + 1
     end if
     
     do shift=-shift_lim, shift_lim
        b(1:n1:2,:,:) = eoshift(a(1:n1/2,:,:),shift,dim=dim)
        call eoshift_0 (a(1:n1/2,:,:), shift=shift, dim=dim, res=c(1:n1:2,:,:))
        if (any (b /= c)) STOP 2
     end do
  end do

end program main
