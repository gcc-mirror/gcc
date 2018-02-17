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


allocate (b(3)[-1:0,2:4,*])

select case (this_image())
  case (1)
    if (this_image(B, dim=1) /= -1) STOP 43
    if (this_image(B, dim=2) /= 2) STOP 44
    if (this_image(B, dim=3) /= 1) STOP 45
    i = 1
    if (this_image(B, dim=i) /= -1) STOP 46
    i = 2
    if (this_image(B, dim=i) /= 2) STOP 47
    i = 3
    if (this_image(B, dim=i) /= 1) STOP 48
    if (any (this_image(B) /= [-1,2,1])) STOP 49

  case (2)
    if (this_image(B, dim=1) /= 0) STOP 50
    if (this_image(B, dim=2) /= 2) STOP 51
    if (this_image(B, dim=3) /= 1) STOP 52
    i = 1
    if (this_image(B, dim=i) /= 0) STOP 53
    i = 2
    if (this_image(B, dim=i) /= 2) STOP 54
    i = 3
    if (this_image(B, dim=i) /= 1) STOP 55
    if (any (this_image(B) /= [0,2,1])) STOP 56

  case (3)
    if (this_image(B, dim=1) /= -1) STOP 57
    if (this_image(B, dim=2) /= 3) STOP 58
    if (this_image(B, dim=3) /= 1) STOP 59
    i = 1
    if (this_image(B, dim=i) /= -1) STOP 60
    i = 2
    if (this_image(B, dim=i) /= 3) STOP 61
    i = 3
    if (this_image(B, dim=i) /= 1) STOP 62
    if (any (this_image(B) /= [-1,3,1])) STOP 63

  case (4)
    if (this_image(B, dim=1) /= 0) STOP 64
    if (this_image(B, dim=2) /= 3) STOP 65
    if (this_image(B, dim=3) /= 1) STOP 66
    i = 1
    if (this_image(B, dim=i) /= 0) STOP 67
    i = 2
    if (this_image(B, dim=i) /= 3) STOP 68
    i = 3
    if (this_image(B, dim=i) /= 1) STOP 69
    if (any (this_image(B) /= [0,3,1])) STOP 70

  case (5)
    if (this_image(B, dim=1) /= -1) STOP 71
    if (this_image(B, dim=2) /= 4) STOP 72
    if (this_image(B, dim=3) /= 1) STOP 73
    i = 1
    if (this_image(B, dim=i) /= -1) STOP 74
    i = 2
    if (this_image(B, dim=i) /= 4) STOP 75
    i = 3
    if (this_image(B, dim=i) /= 1) STOP 76
    if (any (this_image(B) /= [-1,4,1])) STOP 77

  case (6)
    if (this_image(B, dim=1) /= 0) STOP 78
    if (this_image(B, dim=2) /= 4) STOP 79
    if (this_image(B, dim=3) /= 1) STOP 80
    i = 1
    if (this_image(B, dim=i) /= 0) STOP 81
    i = 2
    if (this_image(B, dim=i) /= 4) STOP 82
    i = 3
    if (this_image(B, dim=i) /= 1) STOP 83
    if (any (this_image(B) /= [0,4,1])) STOP 84

  case (7)
    if (this_image(B, dim=1) /= -1) STOP 85
    if (this_image(B, dim=2) /= 2) STOP 86
    if (this_image(B, dim=3) /= 2) STOP 87
    i = 1
    if (this_image(B, dim=i) /= -1) STOP 88
    i = 2
    if (this_image(B, dim=i) /= 2) STOP 89
    i = 3
    if (this_image(B, dim=i) /= 2) STOP 90
    if (any (this_image(B) /= [-1,2,2])) STOP 91

  case (8)
    if (this_image(B, dim=1) /= 0) STOP 92
    if (this_image(B, dim=2) /= 2) STOP 93
    if (this_image(B, dim=3) /= 2) STOP 94
    i = 1
    if (this_image(B, dim=i) /= 0) STOP 95
    i = 2
    if (this_image(B, dim=i) /= 2) STOP 96
    i = 3
    if (this_image(B, dim=i) /= 2) STOP 97
    if (any (this_image(B) /= [0,2,2])) STOP 98
end select

end
