! { dg-do run }
!
! PR fortran/18918
!
! Version for scalar coarrays
!
! this_image(coarray) run test,
! expecially for num_images > 1
!
! Tested are values up to num_images == 8,
! higher values are OK, but not tested for
!
implicit none
integer :: a[2:2, 3:4, 7:*]
integer :: i

if (this_image(A, dim=1) /= 2) call abort()
i = 1
if (this_image(A, dim=i) /= 2) call abort()

select case (this_image())
  case (1)
    if (this_image(A, dim=2) /= 3) call abort()
    if (this_image(A, dim=3) /= 7) call abort()
    i = 2
    if (this_image(A, dim=i) /= 3) call abort()
    i = 3
    if (this_image(A, dim=i) /= 7) call abort()
    if (any (this_image(A) /= [2,3,7])) call abort()

  case (2)
    if (this_image(A, dim=2) /= 4) call abort()
    if (this_image(A, dim=3) /= 7) call abort()
    i = 2
    if (this_image(A, dim=i) /= 4) call abort()
    i = 3
    if (this_image(A, dim=i) /= 7) call abort()
    if (any (this_image(A) /= [2,4,7])) call abort()

  case (3)
    if (this_image(A, dim=2) /= 3) call abort()
    if (this_image(A, dim=3) /= 8) call abort()
    i = 2
    if (this_image(A, dim=i) /= 3) call abort()
    i = 3
    if (this_image(A, dim=i) /= 8) call abort()
    if (any (this_image(A) /= [2,3,8])) call abort()

  case (4)
    if (this_image(A, dim=2) /= 4) call abort()
    if (this_image(A, dim=3) /= 8) call abort()
    i = 2
    if (this_image(A, dim=i) /= 4) call abort()
    i = 3
    if (this_image(A, dim=i) /= 8) call abort()
    if (any (this_image(A) /= [2,4,8])) call abort()

  case (5)
    if (this_image(A, dim=2) /= 3) call abort()
    if (this_image(A, dim=3) /= 9) call abort()
    i = 2
    if (this_image(A, dim=i) /= 3) call abort()
    i = 3
    if (this_image(A, dim=i) /= 9) call abort()
    if (any (this_image(A) /= [2,3,9])) call abort()

  case (6)
    if (this_image(A, dim=2) /= 4) call abort()
    if (this_image(A, dim=3) /= 9) call abort()
    i = 2
    if (this_image(A, dim=i) /= 4) call abort()
    i = 3
    if (this_image(A, dim=i) /= 9) call abort()
    if (any (this_image(A) /= [2,4,9])) call abort()

  case (7)
    if (this_image(A, dim=2) /= 3) call abort()
    if (this_image(A, dim=3) /= 10) call abort()
    i = 2
    if (this_image(A, dim=i) /= 3) call abort()
    i = 3
    if (this_image(A, dim=i) /= 10) call abort()
    if (any (this_image(A) /= [2,3,10])) call abort()

  case (8)
    if (this_image(A, dim=2) /= 4) call abort()
    if (this_image(A, dim=3) /= 10) call abort()
    i = 2
    if (this_image(A, dim=i) /= 4) call abort()
    i = 3
    if (this_image(A, dim=i) /= 10) call abort()
    if (any (this_image(A) /= [2,4,10])) call abort()
end select

contains

subroutine test_image_index
implicit none
integer :: index1, index2, index3
logical :: one

integer, save :: d(2)[-1:3, *]
integer, save :: e(2)[-1:-1, 3:*]

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

end subroutine test_image_index

end
