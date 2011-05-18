! { dg-do run }
!
! Scalar coarray
!
! Run-time test for IMAGE_INDEX with cobounds only known at
! the compile time, suitable for any number of NUM_IMAGES()
! For compile-time cobounds, the -fcoarray=lib version still
! needs to run-time evalulation if image_index returns > 1
! as image_index is 0 if the index would exceed num_images().
!
! Please set num_images() to >= 13, if possible.
!
! PR fortran/18918
!

program test_image_index
implicit none
integer :: index1, index2, index3
logical :: one

integer, save :: d[-1:3, *]
integer, save :: e[-1:-1, 3:*]

one = num_images() == 1

index1 = image_index(d, [-1, 1] )
index2 = image_index(d, [0, 1] )

if (one .and. (index1 /= 1 .or. index2 /= 0)) &
  call abort()
if (.not. one .and. (index1 /= 1 .or. index2 /= 2)) &
  call abort()

index1 = image_index(e, [-1, 3] )
index2 = image_index(e, [-1, 4] )

if (one .and. (index1 /= 1 .or. index2 /= 0)) &
  call abort()
if (.not. one .and. (index1 /= 1 .or. index2 /= 2)) &
  call abort()

call test(1, e, d, e)
call test(2, e, d, e)

contains
subroutine test(n, a, b, c)
  integer :: n
  integer :: a[3*n:3*n, -4*n:-3*n, 88*n:*], b[-1*n:0*n,0*n:*], c[*]

  index1 = image_index(a, [3*n, -4*n, 88*n] )
  index2 = image_index(b, [-1, 0] )
  index3 = image_index(c, [1] )

  if (n == 1) then
    if (index1 /= 1 .or. index2 /= 1 .or. index3 /= 1) call abort()
  else if (num_images() == 1) then
    if (index1 /= 1 .or. index2 /= 0 .or. index3 /= 1) call abort()
  else
    if (index1 /= 1 .or. index2 /= 2 .or. index3 /= 1) call abort()
  end if

  index1 = image_index(a, [3*n, -3*n, 88*n] )
  index2 = image_index(b, [0, 0] )
  index3 = image_index(c, [2] )

  if (one .and. (index1 /= 0 .or. index2 /= 0 .or. index3 /= 0)) &
    call abort()
  if (n == 1 .and. num_images() == 2) then
    if (index1 /= 2 .or. index2 /= 2 .or. index3 /= 2) &
      call abort()
  else if (n == 2 .and. num_images() == 2) then 
    if (index1 /= 0 .or. index2 /= 0 .or. index3 /= 2) &
      call abort()
  end if
end subroutine test
end program test_image_index
