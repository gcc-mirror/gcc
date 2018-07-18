! { dg-do  run }
! Check that eoshift works for three-dimensional arrays.
module x
  implicit none
contains
  subroutine eoshift_2 (array, shift, boundary, dim, res)
    real, dimension(:,:,:), intent(in) :: array
    real, dimension(:,:,:), intent(out) :: res
    integer, value :: shift
    real, optional, dimension(:,:), intent(in) :: boundary
    integer, optional, intent(in) :: dim
    integer :: s1, s2, s3
    integer :: n1, n2, n3

    real :: b
    integer :: d

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
                b = boundary(s2,s3)
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
                b = boundary(s2,s3)
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
                   b = boundary(s1,s3)
                   res(s1,s2,s3) = b
                end do
             end do
          end do
       else
          shift = max(shift, -n2)
          do s3=1,n3
             do s2=1,-shift
                do s1=1,n1
                   b = boundary(s1,s3)
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
                   b = boundary(s1,s2)
                   res(s1,s2,s3) = b
                end do
             end do
          end do
       else
          shift = max(shift, -n3)
          do s3=1,-shift
             do s2=1,n2
                do s1=1,n1
                   b = boundary(s1,s2)
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
  end subroutine eoshift_2
end module x

program main
  use x
  implicit none
  integer, parameter :: n1=20,n2=30,n3=40
  real, dimension(n1,n2,n3) :: a,b,c
  real, dimension(2*n1,n2,n3) :: a2,c2
  integer :: dim, shift, shift_lim
  real, dimension(n2,n3), target :: b1
  real, dimension(n1,n3), target :: b2
  real, dimension(n1,n2), target :: b3
  real, dimension(:,:), pointer :: bp

  call random_number(a)
  call random_number (b1)
  call random_number (b2)
  call random_number (b3)
  do dim=1,3
     if (dim == 1) then
        shift_lim = n1 + 1
        bp => b1
     else if (dim == 2) then
        shift_lim = n2 + 1
        bp => b2
     else
        shift_lim = n3 + 1
        bp => b3
     end if
     do shift=-shift_lim, shift_lim
        b = eoshift(a,shift,dim=dim, boundary=bp)
        call eoshift_2 (a, shift=shift, dim=dim, boundary=bp, res=c)
        if (any (b /= c)) then
           print *,"dim = ", dim, "shift = ", shift
           print *,b
           print *,c
           STOP 1
        end if
        a2 = 42.
        a2(1:2*n1:2,:,:) = a
        b = eoshift(a2(1:2*n1:2,:,:), shift, dim=dim, boundary=bp)
        if (any (b /= c)) then
           STOP 2
        end if
        c2 = 43.
        c2(1:2*n1:2,:,:) = eoshift(a,shift,dim=dim, boundary=bp)
        if (any(c2(1:2*n1:2,:,:) /= c)) then
           STOP 3
        end if
        if (any(c2(2:2*n1:2,:,:) /= 43)) then
           STOP 4
        end if
     end do
  end do
end program main
