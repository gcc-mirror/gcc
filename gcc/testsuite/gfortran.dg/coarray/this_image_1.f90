! { dg-do run }
!
! PR fortran/18918
!
! this_image(coarray) run test,
! expecially for num_images > 1
!
! Tested are values up to num_images == 8,
! higher values are OK, but not tested for
!
implicit none
integer :: a(1)[2:2, 3:4, 7:*]
integer :: b(:)[:, :,:]
allocatable :: b
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


allocate (b(3)[-1:0,2:4,*])

select case (this_image())
  case (1)
    if (this_image(B, dim=1) /= -1) call abort()
    if (this_image(B, dim=2) /= 2) call abort()
    if (this_image(B, dim=3) /= 1) call abort()
    i = 1
    if (this_image(B, dim=i) /= -1) call abort()
    i = 2
    if (this_image(B, dim=i) /= 2) call abort()
    i = 3
    if (this_image(B, dim=i) /= 1) call abort()
    if (any (this_image(B) /= [-1,2,1])) call abort()

  case (2)
    if (this_image(B, dim=1) /= 0) call abort()
    if (this_image(B, dim=2) /= 2) call abort()
    if (this_image(B, dim=3) /= 1) call abort()
    i = 1
    if (this_image(B, dim=i) /= 0) call abort()
    i = 2
    if (this_image(B, dim=i) /= 2) call abort()
    i = 3
    if (this_image(B, dim=i) /= 1) call abort()
    if (any (this_image(B) /= [0,2,1])) call abort()

  case (3)
    if (this_image(B, dim=1) /= -1) call abort()
    if (this_image(B, dim=2) /= 3) call abort()
    if (this_image(B, dim=3) /= 1) call abort()
    i = 1
    if (this_image(B, dim=i) /= -1) call abort()
    i = 2
    if (this_image(B, dim=i) /= 3) call abort()
    i = 3
    if (this_image(B, dim=i) /= 1) call abort()
    if (any (this_image(B) /= [-1,3,1])) call abort()

  case (4)
    if (this_image(B, dim=1) /= 0) call abort()
    if (this_image(B, dim=2) /= 3) call abort()
    if (this_image(B, dim=3) /= 1) call abort()
    i = 1
    if (this_image(B, dim=i) /= 0) call abort()
    i = 2
    if (this_image(B, dim=i) /= 3) call abort()
    i = 3
    if (this_image(B, dim=i) /= 1) call abort()
    if (any (this_image(B) /= [0,3,1])) call abort()

  case (5)
    if (this_image(B, dim=1) /= -1) call abort()
    if (this_image(B, dim=2) /= 4) call abort()
    if (this_image(B, dim=3) /= 1) call abort()
    i = 1
    if (this_image(B, dim=i) /= -1) call abort()
    i = 2
    if (this_image(B, dim=i) /= 4) call abort()
    i = 3
    if (this_image(B, dim=i) /= 1) call abort()
    if (any (this_image(B) /= [-1,4,1])) call abort()

  case (6)
    if (this_image(B, dim=1) /= 0) call abort()
    if (this_image(B, dim=2) /= 4) call abort()
    if (this_image(B, dim=3) /= 1) call abort()
    i = 1
    if (this_image(B, dim=i) /= 0) call abort()
    i = 2
    if (this_image(B, dim=i) /= 4) call abort()
    i = 3
    if (this_image(B, dim=i) /= 1) call abort()
    if (any (this_image(B) /= [0,4,1])) call abort()

  case (7)
    if (this_image(B, dim=1) /= -1) call abort()
    if (this_image(B, dim=2) /= 2) call abort()
    if (this_image(B, dim=3) /= 2) call abort()
    i = 1
    if (this_image(B, dim=i) /= -1) call abort()
    i = 2
    if (this_image(B, dim=i) /= 2) call abort()
    i = 3
    if (this_image(B, dim=i) /= 2) call abort()
    if (any (this_image(B) /= [-1,2,2])) call abort()

  case (8)
    if (this_image(B, dim=1) /= 0) call abort()
    if (this_image(B, dim=2) /= 2) call abort()
    if (this_image(B, dim=3) /= 2) call abort()
    i = 1
    if (this_image(B, dim=i) /= 0) call abort()
    i = 2
    if (this_image(B, dim=i) /= 2) call abort()
    i = 3
    if (this_image(B, dim=i) /= 2) call abort()
    if (any (this_image(B) /= [0,2,2])) call abort()
end select

end
