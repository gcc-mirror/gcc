! { dg-do run }
! { dg-options "-std=gnu" }
! PR fortran/29391
! This file is here to check that LBOUND and UBOUND return correct values
!
! Contributed by Francois-Xavier Coudert (coudert@clipper.ens.fr)
  implicit none
  integer :: i(-1:1,-1:1) = 0
  integer :: j(-1:2) = 0
  integer :: u(7,4,2,9)

  call foo(u,4)
  call jackal(-1,-8)
  call jackal(-1,8)

  if (any(lbound(i(-1:1,-1:1)) /= 1)) STOP 1
  if (lbound(i(-1:1,-1:1), 1) /= 1) STOP 2
  if (lbound(i(-1:1,-1:1), 2) /= 1) STOP 3

  if (any(ubound(i(-1:1,-1:1)) /= 3)) STOP 4
  if (ubound(i(-1:1,-1:1), 1) /= 3) STOP 5
  if (ubound(i(-1:1,-1:1), 2) /= 3) STOP 6

  if (any(lbound(i(:,:)) /= 1)) STOP 7
  if (lbound(i(:,:), 1) /= 1) STOP 8
  if (lbound(i(:,:), 2) /= 1) STOP 9

  if (any(ubound(i(:,:)) /= 3)) STOP 10
  if (ubound(i(:,:), 1) /= 3) STOP 11
  if (ubound(i(:,:), 2) /= 3) STOP 12

  if (any(lbound(i(0:,-1:)) /= 1)) STOP 13
  if (lbound(i(0:,-1:), 1) /= 1) STOP 14
  if (lbound(i(0:,-1:), 2) /= 1) STOP 15

  if (any(ubound(i(0:,-1:)) /= [2,3])) STOP 16
  if (ubound(i(0:,-1:), 1) /= 2) STOP 17
  if (ubound(i(0:,-1:), 2) /= 3) STOP 18

  if (any(lbound(i(:0,:0)) /= 1)) STOP 19
  if (lbound(i(:0,:0), 1) /= 1) STOP 20
  if (lbound(i(:0,:0), 2) /= 1) STOP 21

  if (any(ubound(i(:0,:0)) /= 2)) STOP 22
  if (ubound(i(:0,:0), 1) /= 2) STOP 23
  if (ubound(i(:0,:0), 2) /= 2) STOP 24

  if (any(lbound(transpose(i)) /= 1)) STOP 25
  if (lbound(transpose(i), 1) /= 1) STOP 26
  if (lbound(transpose(i), 2) /= 1) STOP 27

  if (any(ubound(transpose(i)) /= 3)) STOP 28
  if (ubound(transpose(i), 1) /= 3) STOP 29
  if (ubound(transpose(i), 2) /= 3) STOP 30

  if (any(lbound(reshape(i,[2,2])) /= 1)) STOP 31
  if (lbound(reshape(i,[2,2]), 1) /= 1) STOP 32
  if (lbound(reshape(i,[2,2]), 2) /= 1) STOP 33

  if (any(ubound(reshape(i,[2,2])) /= 2)) STOP 34
  if (ubound(reshape(i,[2,2]), 1) /= 2) STOP 35
  if (ubound(reshape(i,[2,2]), 2) /= 2) STOP 36

  if (any(lbound(cshift(i,-1)) /= 1)) STOP 37
  if (lbound(cshift(i,-1), 1) /= 1) STOP 38
  if (lbound(cshift(i,-1), 2) /= 1) STOP 39

  if (any(ubound(cshift(i,-1)) /= 3)) STOP 40
  if (ubound(cshift(i,-1), 1) /= 3) STOP 41
  if (ubound(cshift(i,-1), 2) /= 3) STOP 42

  if (any(lbound(eoshift(i,-1)) /= 1)) STOP 43
  if (lbound(eoshift(i,-1), 1) /= 1) STOP 44
  if (lbound(eoshift(i,-1), 2) /= 1) STOP 45

  if (any(ubound(eoshift(i,-1)) /= 3)) STOP 46
  if (ubound(eoshift(i,-1), 1) /= 3) STOP 47
  if (ubound(eoshift(i,-1), 2) /= 3) STOP 48

  if (any(lbound(spread(i,1,2)) /= 1)) STOP 49
  if (lbound(spread(i,1,2), 1) /= 1) STOP 50
  if (lbound(spread(i,1,2), 2) /= 1) STOP 51

  if (any(ubound(spread(i,1,2)) /= [2,3,3])) STOP 52
  if (ubound(spread(i,1,2), 1) /= 2) STOP 53
  if (ubound(spread(i,1,2), 2) /= 3) STOP 54
  if (ubound(spread(i,1,2), 3) /= 3) STOP 55

  if (any(lbound(maxloc(i)) /= 1)) STOP 56
  if (lbound(maxloc(i), 1) /= 1) STOP 57

  if (any(ubound(maxloc(i)) /= 2)) STOP 58
  if (ubound(maxloc(i), 1) /= 2) STOP 59

  if (any(lbound(minloc(i)) /= 1)) STOP 60
  if (lbound(minloc(i), 1) /= 1) STOP 61

  if (any(ubound(minloc(i)) /= 2)) STOP 62
  if (ubound(minloc(i), 1) /= 2) STOP 63

  if (any(lbound(maxval(i,2)) /= 1)) STOP 64
  if (lbound(maxval(i,2), 1) /= 1) STOP 65

  if (any(ubound(maxval(i,2)) /= 3)) STOP 66
  if (ubound(maxval(i,2), 1) /= 3) STOP 67

  if (any(lbound(minval(i,2)) /= 1)) STOP 68
  if (lbound(minval(i,2), 1) /= 1) STOP 69

  if (any(ubound(minval(i,2)) /= 3)) STOP 70
  if (ubound(minval(i,2), 1) /= 3) STOP 71

  if (any(lbound(any(i==1,2)) /= 1)) STOP 72
  if (lbound(any(i==1,2), 1) /= 1) STOP 73

  if (any(ubound(any(i==1,2)) /= 3)) STOP 74
  if (ubound(any(i==1,2), 1) /= 3) STOP 75

  if (any(lbound(count(i==1,2)) /= 1)) STOP 76
  if (lbound(count(i==1,2), 1) /= 1) STOP 77

  if (any(ubound(count(i==1,2)) /= 3)) STOP 78
  if (ubound(count(i==1,2), 1) /= 3) STOP 79

  if (any(lbound(merge(i,i,.true.)) /= 1)) STOP 80
  if (lbound(merge(i,i,.true.), 1) /= 1) STOP 81
  if (lbound(merge(i,i,.true.), 2) /= 1) STOP 82

  if (any(ubound(merge(i,i,.true.)) /= 3)) STOP 83
  if (ubound(merge(i,i,.true.), 1) /= 3) STOP 84
  if (ubound(merge(i,i,.true.), 2) /= 3) STOP 85

  if (any(lbound(lbound(i)) /= 1)) STOP 86
  if (lbound(lbound(i), 1) /= 1) STOP 87

  if (any(ubound(lbound(i)) /= 2)) STOP 88
  if (ubound(lbound(i), 1) /= 2) STOP 89

  if (any(lbound(ubound(i)) /= 1)) STOP 90
  if (lbound(ubound(i), 1) /= 1) STOP 91

  if (any(ubound(ubound(i)) /= 2)) STOP 92
  if (ubound(ubound(i), 1) /= 2) STOP 93

  if (any(lbound(shape(i)) /= 1)) STOP 94
  if (lbound(shape(i), 1) /= 1) STOP 95

  if (any(ubound(shape(i)) /= 2)) STOP 96
  if (ubound(shape(i), 1) /= 2) STOP 97

  if (any(lbound(product(i,2)) /= 1)) STOP 98
  if (any(ubound(product(i,2)) /= 3)) STOP 99
  if (any(lbound(sum(i,2)) /= 1)) STOP 100
  if (any(ubound(sum(i,2)) /= 3)) STOP 101
  if (any(lbound(matmul(i,i)) /= 1)) STOP 102
  if (any(ubound(matmul(i,i)) /= 3)) STOP 103
  if (any(lbound(pack(i,.true.)) /= 1)) STOP 104
  if (any(ubound(pack(i,.true.)) /= 9)) STOP 105
  if (any(lbound(unpack(j,[.true.],[2])) /= 1)) STOP 106
  if (any(ubound(unpack(j,[.true.],[2])) /= 1)) STOP 107

  call sub1(i,3)
  call sub1(reshape([7,9,4,6,7,9],[3,2]),3)
  call sub2

contains

  subroutine sub1(a,n)
    integer :: n, a(2:n+1,4:*)

    if (any([lbound(a,1), lbound(a,2)] /= [2, 4])) STOP 108
    if (any(lbound(a) /= [2, 4])) STOP 109
  end subroutine sub1

  subroutine sub2
    integer :: x(3:2, 1:2)

    if (size(x) /= 0) STOP 110
    if (lbound (x, 1) /= 1 .or. lbound(x, 2) /= 1) STOP 111
    if (any (lbound (x) /= [1, 1])) STOP 112
    if (ubound (x, 1) /= 0 .or. ubound(x, 2) /= 2) STOP 113
    if (any (ubound (x) /= [0, 2])) STOP 114
  end subroutine sub2

  subroutine sub3
    integer :: x(4:5, 1:2)

    if (size(x) /= 0) STOP 115
    if (lbound (x, 1) /= 4 .or. lbound(x, 2) /= 1) STOP 116
    if (any (lbound (x) /= [4, 1])) STOP 117
    if (ubound (x, 1) /= 4 .or. ubound(x, 2) /= 2) STOP 118
    if (any (ubound (x) /= [4, 2])) STOP 119
  end subroutine sub3

  subroutine foo (x,n)
    integer :: n
    integer :: x(7,n,2,*)

    if (ubound(x,1) /= 7 .or. ubound(x,2) /= 4 .or. ubound(x,3) /= 2) STOP 120
  end subroutine foo

  subroutine jackal (b, c)
    integer :: b, c
    integer :: soda(b:c, 3:4)

    if (b > c) then
      if (size(soda) /= 0) STOP 121
      if (lbound (soda, 1) /= 1 .or. ubound (soda, 1) /= 0) STOP 122
    else
      if (size(soda) /= 2*(c-b+1)) STOP 123
      if (lbound (soda, 1) /= b .or. ubound (soda, 1) /= c) STOP 124
    end if

    if (lbound (soda, 2) /= 3 .or. ubound (soda, 2) /= 4) STOP 125
    if (any (lbound (soda) /= [lbound(soda,1), lbound(soda,2)])) STOP 126
    if (any (ubound (soda) /= [ubound(soda,1), ubound(soda,2)])) STOP 127

  end subroutine jackal

end
