! { dg-do run }
!
! CO_REDUCE
!
implicit none (type, external)
intrinsic :: co_reduce
integer :: stat
integer :: i4, i4_2, i

i4 = 21 * this_image()
i4_2 = 21
do i = 2, num_images()
  i4_2 = i4_2 * 21 * i
end do
call co_reduce(i4, op_i4, stat=stat)
if (stat /= 0) STOP 1
if (i4_2 /= i4) STOP 2

contains
  pure integer function op_i4(a,b)
    integer, value :: a, b
    op_i4 = a * b
  end function op_i4
end
