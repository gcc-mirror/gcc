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

if (this_image(A, dim=1) /= 2) STOP 1
i = 1
if (this_image(A, dim=i) /= 2) STOP 2

select case (this_image())
  case (1)
    if (this_image(A, dim=2) /= 3) STOP 3
    if (this_image(A, dim=3) /= 7) STOP 4
    i = 2
    if (this_image(A, dim=i) /= 3) STOP 5
    i = 3
    if (this_image(A, dim=i) /= 7) STOP 6
    if (any (this_image(A) /= [2,3,7])) STOP 7

  case (2)
    if (this_image(A, dim=2) /= 4) STOP 8
    if (this_image(A, dim=3) /= 7) STOP 9
    i = 2
    if (this_image(A, dim=i) /= 4) STOP 10
    i = 3
    if (this_image(A, dim=i) /= 7) STOP 11
    if (any (this_image(A) /= [2,4,7])) STOP 12

  case (3)
    if (this_image(A, dim=2) /= 3) STOP 13
    if (this_image(A, dim=3) /= 8) STOP 14
    i = 2
    if (this_image(A, dim=i) /= 3) STOP 15
    i = 3
    if (this_image(A, dim=i) /= 8) STOP 16
    if (any (this_image(A) /= [2,3,8])) STOP 17

  case (4)
    if (this_image(A, dim=2) /= 4) STOP 18
    if (this_image(A, dim=3) /= 8) STOP 19
    i = 2
    if (this_image(A, dim=i) /= 4) STOP 20
    i = 3
    if (this_image(A, dim=i) /= 8) STOP 21
    if (any (this_image(A) /= [2,4,8])) STOP 22

  case (5)
    if (this_image(A, dim=2) /= 3) STOP 23
    if (this_image(A, dim=3) /= 9) STOP 24
    i = 2
    if (this_image(A, dim=i) /= 3) STOP 25
    i = 3
    if (this_image(A, dim=i) /= 9) STOP 26
    if (any (this_image(A) /= [2,3,9])) STOP 27

  case (6)
    if (this_image(A, dim=2) /= 4) STOP 28
    if (this_image(A, dim=3) /= 9) STOP 29
    i = 2
    if (this_image(A, dim=i) /= 4) STOP 30
    i = 3
    if (this_image(A, dim=i) /= 9) STOP 31
    if (any (this_image(A) /= [2,4,9])) STOP 32

  case (7)
    if (this_image(A, dim=2) /= 3) STOP 33
    if (this_image(A, dim=3) /= 10) STOP 34
    i = 2
    if (this_image(A, dim=i) /= 3) STOP 35
    i = 3
    if (this_image(A, dim=i) /= 10) STOP 36
    if (any (this_image(A) /= [2,3,10])) STOP 37

  case (8)
    if (this_image(A, dim=2) /= 4) STOP 38
    if (this_image(A, dim=3) /= 10) STOP 39
    i = 2
    if (this_image(A, dim=i) /= 4) STOP 40
    i = 3
    if (this_image(A, dim=i) /= 10) STOP 41
    if (any (this_image(A) /= [2,4,10])) STOP 42
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
  STOP 43
if (.not. one .and. (index1 /= 1 .or. index2 /= 2)) &
  STOP 44

index1 = image_index(e, [-1, 3] )
index2 = image_index(e, [-1, 4] )

if (one .and. (index1 /= 1 .or. index2 /= 0)) &
  STOP 45
if (.not. one .and. (index1 /= 1 .or. index2 /= 2)) &
  STOP 46

end subroutine test_image_index

end
