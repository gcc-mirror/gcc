! { dg-do run }
! { dg-options "-fcoarray=single" }
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

integer, allocatable :: a(:)[:,:,:], b(:)[:,:], c(:,:)[:]
integer, save :: d(2)[-1:3, *]
integer, save :: e(2)[-1:-1, 3:*]

one = num_images() == 1

allocate(a(1)[3:3, -4:-3, 88:*])
allocate(b(2)[-1:0,0:*])
allocate(c(3,3)[*])

index1 = image_index(a, [3, -4, 88] )
index2 = image_index(b, [-1, 0] )
index3 = image_index(c, [1] )
if (index1 /= 1 .or. index2 /= 1 .or. index3 /= 1) call abort()


index1 = image_index(a, [3, -3, 88] )
index2 = image_index(b, [0, 0] )
index3 = image_index(c, [2] )

if (one .and. (index1 /= 0 .or. index2 /= 0 .or. index3 /= 0)) &
  call abort()
if (.not. one .and. (index1 /= 2 .or. index2 /= 2 .or. index3 /= 2)) &
  call abort()


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

call test(1, a,b,c)

! The following test is in honour of the F2008 standard:
deallocate(a)
allocate(a (10) [10, 0:9, 0:*])

index1 = image_index(a, [1, 0, 0] )
index2 = image_index(a, [3, 1, 2] )  ! = 213, yeah!
index3 = image_index(a, [3, 1, 0] )  ! = 13

if (num_images() < 13 .and. (index1 /= 1 .or. index2 /= 0 .or. index3 /= 0)) &
  call abort()
if (num_images() >= 213 .and. (index1 /= 1 .or. index2 /= 213 .or. index3 /= 13)) &
  call abort()
if (num_images() >= 13 .and. (index1 /= 1 .or. index2 /= 0 .or. index3 /= 13)) &
  call abort()


contains
subroutine test(n, a, b, c)
  integer :: n
  integer :: a(1)[3*n:3*n, -4*n:-3*n, 88*n:*], b(2)[-1*n:0*n,0*n:*], c(3*n,3*n)[*]

  index1 = image_index(a, [3, -4, 88] )
  index2 = image_index(b, [-1, 0] )
  index3 = image_index(c, [1] )
  if (index1 /= 1 .or. index2 /= 1 .or. index3 /= 1) call abort()


  index1 = image_index(a, [3, -3, 88] )
  index2 = image_index(b, [0, 0] )
  index3 = image_index(c, [2] )

  if (one .and. (index1 /= 0 .or. index2 /= 0 .or. index3 /= 0)) &
    call abort()
  if (.not. one .and. (index1 /= 2 .or. index2 /= 2 .or. index3 /= 2)) &
    call abort()
end subroutine test
end program test_image_index
