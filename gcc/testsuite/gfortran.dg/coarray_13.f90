! { dg-do run }
! { dg-options "-fcoarray=single -fcheck=bounds" }
!
! Coarray support -- allocatable array coarrays
!                 -- intrinsic procedures
! PR fortran/18918
! PR fortran/43931
!
program test
  implicit none
  integer,allocatable :: B(:)[:]

  call one()
  call two()
  allocate(B(3)[-4:*])
  call three(3,B,1)
  call three_a(3,B)
  call three_b(3,B)
  call four(B)
  call five()
contains
  subroutine one()
    integer, allocatable :: a(:)[:,:,:]
    allocate(a(1)[-4:9,8,4:*])
 
    if (this_image(a,dim=1) /= -4_8) STOP 1
    if (lcobound  (a,dim=1) /= -4_8) STOP 2
    if (ucobound  (a,dim=1) /=  9_8) STOP 3
 
    if (this_image(a,dim=2) /=  1_8) STOP 4
    if (lcobound  (a,dim=2) /=  1_8) STOP 5
    if (ucobound  (a,dim=2) /=  8_8) STOP 6
 
    if (this_image(a,dim=3) /= 4_8) STOP 7
    if (lcobound  (a,dim=3) /= 4_8) STOP 8
    if (ucobound  (a,dim=3) /= 4_8) STOP 9
 
    if (any(this_image(a) /= [-4_8, 1_8, 4_8])) STOP 10
    if (any(lcobound  (a) /= [-4_8, 1_8, 4_8])) STOP 11
    if (any(ucobound  (a) /= [9_8, 8_8, 4_8])) STOP 12
  end subroutine one

  subroutine two()
    integer, allocatable :: a(:)[:,:,:]
    allocate(a(1)[-4:9,8,4:*])

    if (this_image(a,dim=1) /= -4) STOP 13
    if (lcobound  (a,dim=1) /= -4) STOP 14
    if (ucobound  (a,dim=1) /=  9) STOP 15

    if (this_image(a,dim=2) /=  1) STOP 16
    if (lcobound  (a,dim=2) /=  1) STOP 17
    if (ucobound  (a,dim=2) /=  8) STOP 18

    if (this_image(a,dim=3) /= 4) STOP 19
    if (lcobound  (a,dim=3) /= 4) STOP 20
    if (ucobound  (a,dim=3) /= 4) STOP 21

    if (any(this_image(a) /= [-4, 1, 4])) STOP 22
    if (any(lcobound  (a) /= [-4, 1, 4])) STOP 23
    if (any(ucobound  (a) /= [9, 8, 4])) STOP 24
  end subroutine two

  subroutine three(n,A, n2)
    integer :: n, n2
    integer :: A(3)[n:*]

    A(1) = 42
    if (A(1) /= 42) STOP 25
    A(1)[n2] = -42
    if (A(1)[n2] /= -42) STOP 26

    if (this_image(A,dim=1) /= n) STOP 27
    if (lcobound  (A,dim=1) /= n) STOP 28
    if (ucobound  (A,dim=1) /= n) STOP 29

    if (any(this_image(A) /= n)) STOP 30
    if (any(lcobound  (A) /= n)) STOP 31
    if (any(ucobound  (A) /= n)) STOP 32
  end subroutine three

  subroutine three_a(n,A)
    integer :: n
    integer :: A(3)[n+2:n+5,n-1:*]

    A(1) = 42
    if (A(1) /= 42) STOP 33
    A(1)[4,n] = -42
    if (A(1)[4,n] /= -42) STOP 34

    if (this_image(A,dim=1) /= n+2) STOP 35
    if (lcobound  (A,dim=1) /= n+2) STOP 36
    if (ucobound  (A,dim=1) /= n+5) STOP 37

    if (this_image(A,dim=2) /= n-1) STOP 38
    if (lcobound  (A,dim=2) /= n-1) STOP 39
    if (ucobound  (A,dim=2) /= n-1) STOP 40

    if (any(this_image(A) /= [n+2,n-1])) STOP 41
    if (any(lcobound  (A) /= [n+2,n-1])) STOP 42
    if (any(ucobound  (A) /= [n+5,n-1])) STOP 43
  end subroutine three_a

  subroutine three_b(n,A)
    integer :: n
    integer :: A(-1:3,0:4,-2:5,-4:7)[n+2:n+5,n-1:*]

    A(-1,0,-2,-4) = 42
    if (A(-1,0,-2,-4) /= 42) STOP 44
    A(1,0,-2,-4) = 99
    if (A(1,0,-2,-4) /= 99) STOP 45

    if (this_image(A,dim=1) /= n+2) STOP 46
    if (lcobound  (A,dim=1) /= n+2) STOP 47
    if (ucobound  (A,dim=1) /= n+5) STOP 48

    if (this_image(A,dim=2) /= n-1) STOP 49
    if (lcobound  (A,dim=2) /= n-1) STOP 50
    if (ucobound  (A,dim=2) /= n-1) STOP 51

    if (any(this_image(A) /= [n+2,n-1])) STOP 52
    if (any(lcobound  (A) /= [n+2,n-1])) STOP 53
    if (any(ucobound  (A) /= [n+5,n-1])) STOP 54
  end subroutine three_b

  subroutine four(A)
    integer, allocatable :: A(:)[:]
    if (this_image(A,dim=1) /= -4_8) STOP 55
    if (lcobound  (A,dim=1) /= -4_8) STOP 56
    if (ucobound  (A,dim=1) /= -4_8) STOP 57
  end subroutine four

  subroutine five()
    integer, save :: foo(2)[5:7,4:*]
    integer :: i

    i = 1
    foo(1)[5,4] = 42
    if (foo(1)[5,4] /= 42) STOP 58
    if (this_image(foo,dim=i) /= 5) STOP 59
    if (lcobound(foo,dim=i) /= 5) STOP 60
    if (ucobound(foo,dim=i) /= 7) STOP 61

    i = 2
    if (this_image(foo,dim=i) /= 4) STOP 62
    if (lcobound(foo,dim=i) /= 4) STOP 63
    if (ucobound(foo,dim=i) /= 4) STOP 64
  end subroutine five
end program test
