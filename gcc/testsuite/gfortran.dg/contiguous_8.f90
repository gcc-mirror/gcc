! { dg-do  run }
! PR 56789 - packing / unpacking of contiguous arguments
! did not happen.

module my_module
  implicit none
contains
  subroutine cont_arg(a)
    real, contiguous :: a(:,:)
    integer :: i,j
    do j=1,size(a,2)
       do i=1,size(a,1)
          a(i,j) = i+10*j
       end do
    end do
  end subroutine cont_arg
  subroutine cont_pointer_arg (a)
    integer, pointer, contiguous :: a(:)
    call assumed_size(a)
    call assumed_size(a(::1))
    call assumed_size_2(a(::2))
  end subroutine cont_pointer_arg

  subroutine assumed_size(y)
    integer, dimension(*) :: y
    if (y(2) /= 2 .or. y(3) /= 3 .or. y(4) /= 4 .or. y(5) /= 5 .or. y(6) /= 6) &
            stop 2
  end subroutine assumed_size

  subroutine assumed_size_2(y)
    integer, dimension(*) :: y
    if (y(1) /= 1 .or. y(2) /= 3 .or. y(3) /= 5) stop 3
  end subroutine assumed_size_2

  subroutine cont_assumed_shape(x)
    integer, dimension(:), contiguous :: x
    if (size(x,1) == 8) then
       if (any(x /= [1,2,3,4,5,6,7,8])) stop 4
    else
       if (any(x /= [1,3,5,7])) stop 5
    end if
  end subroutine cont_assumed_shape
end module my_module

program main
  use my_module
  implicit none
  real, dimension(5,5) :: a
  real, dimension(5,5) :: res
  integer, dimension(8), target :: t
  integer, dimension(:), pointer, contiguous :: p
  res = reshape([11., 1.,12., 1.,13.,&
                  1., 1., 1., 1., 1.,&
                 21., 1.,22., 1.,23.,&
                  1., 1., 1., 1., 1.,&
                 31., 1.,32., 1., 33.], shape(res))
  a = 1.
  call cont_arg(a(1:5:2,1:5:2))
  if (any(a /= res)) stop 1
  t = [1,2,3,4,5,6,7,8]
  p => t
  call cont_pointer_arg(p)
  call cont_assumed_shape (t)
  call cont_assumed_shape (t(::2))
end program main
