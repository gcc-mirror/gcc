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
 
    if (this_image(a,dim=1) /= -4_8) call abort()
    if (lcobound  (a,dim=1) /= -4_8) call abort()
    if (ucobound  (a,dim=1) /=  9_8) call abort()
 
    if (this_image(a,dim=2) /=  1_8) call abort()
    if (lcobound  (a,dim=2) /=  1_8) call abort()
    if (ucobound  (a,dim=2) /=  8_8) call abort()
 
    if (this_image(a,dim=3) /= 4_8) call abort()
    if (lcobound  (a,dim=3) /= 4_8) call abort()
    if (ucobound  (a,dim=3) /= 4_8) call abort()
 
    if (any(this_image(a) /= [-4_8, 1_8, 4_8])) call abort()
    if (any(lcobound  (a) /= [-4_8, 1_8, 4_8])) call abort()
    if (any(ucobound  (a) /= [9_8, 8_8, 4_8])) call abort()
  end subroutine one

  subroutine two()
    integer, allocatable :: a(:)[:,:,:]
    allocate(a(1)[-4:9,8,4:*])

    if (this_image(a,dim=1) /= -4) call abort()
    if (lcobound  (a,dim=1) /= -4) call abort()
    if (ucobound  (a,dim=1) /=  9) call abort()

    if (this_image(a,dim=2) /=  1) call abort()
    if (lcobound  (a,dim=2) /=  1) call abort()
    if (ucobound  (a,dim=2) /=  8) call abort()

    if (this_image(a,dim=3) /= 4) call abort()
    if (lcobound  (a,dim=3) /= 4) call abort()
    if (ucobound  (a,dim=3) /= 4) call abort()

    if (any(this_image(a) /= [-4, 1, 4])) call abort()
    if (any(lcobound  (a) /= [-4, 1, 4])) call abort()
    if (any(ucobound  (a) /= [9, 8, 4])) call abort()
  end subroutine two

  subroutine three(n,A, n2)
    integer :: n, n2
    integer :: A(3)[n:*]

    A(1) = 42
    if (A(1) /= 42) call abort()
    A(1)[n2] = -42
    if (A(1)[n2] /= -42) call abort()

    if (this_image(A,dim=1) /= n) call abort()
    if (lcobound  (A,dim=1) /= n) call abort()
    if (ucobound  (A,dim=1) /= n) call abort()

    if (any(this_image(A) /= n)) call abort()
    if (any(lcobound  (A) /= n)) call abort()
    if (any(ucobound  (A) /= n)) call abort()
  end subroutine three

  subroutine three_a(n,A)
    integer :: n
    integer :: A(3)[n+2:n+5,n-1:*]

    A(1) = 42
    if (A(1) /= 42) call abort()
    A(1)[4,n] = -42
    if (A(1)[4,n] /= -42) call abort()

    if (this_image(A,dim=1) /= n+2) call abort()
    if (lcobound  (A,dim=1) /= n+2) call abort()
    if (ucobound  (A,dim=1) /= n+5) call abort()

    if (this_image(A,dim=2) /= n-1) call abort()
    if (lcobound  (A,dim=2) /= n-1) call abort()
    if (ucobound  (A,dim=2) /= n-1) call abort()

    if (any(this_image(A) /= [n+2,n-1])) call abort()
    if (any(lcobound  (A) /= [n+2,n-1])) call abort()
    if (any(ucobound  (A) /= [n+5,n-1])) call abort()
  end subroutine three_a

  subroutine three_b(n,A)
    integer :: n
    integer :: A(-1:3,0:4,-2:5,-4:7)[n+2:n+5,n-1:*]

    A(-1,0,-2,-4) = 42
    if (A(-1,0,-2,-4) /= 42) call abort()
    A(1,0,-2,-4) = 99
    if (A(1,0,-2,-4) /= 99) call abort()

    if (this_image(A,dim=1) /= n+2) call abort()
    if (lcobound  (A,dim=1) /= n+2) call abort()
    if (ucobound  (A,dim=1) /= n+5) call abort()

    if (this_image(A,dim=2) /= n-1) call abort()
    if (lcobound  (A,dim=2) /= n-1) call abort()
    if (ucobound  (A,dim=2) /= n-1) call abort()

    if (any(this_image(A) /= [n+2,n-1])) call abort()
    if (any(lcobound  (A) /= [n+2,n-1])) call abort()
    if (any(ucobound  (A) /= [n+5,n-1])) call abort()
  end subroutine three_b

  subroutine four(A)
    integer, allocatable :: A(:)[:]
    if (this_image(A,dim=1) /= -4_8) call abort()
    if (lcobound  (A,dim=1) /= -4_8) call abort()
    if (ucobound  (A,dim=1) /= -4_8) call abort()
  end subroutine four

  subroutine five()
    integer, save :: foo(2)[5:7,4:*]
    integer :: i

    i = 1
    foo(1)[5,4] = 42
    if (foo(1)[5,4] /= 42) call abort()
    if (this_image(foo,dim=i) /= 5) call abort()
    if (lcobound(foo,dim=i) /= 5) call abort()
    if (ucobound(foo,dim=i) /= 7) call abort()

    i = 2
    if (this_image(foo,dim=i) /= 4) call abort()
    if (lcobound(foo,dim=i) /= 4) call abort()
    if (ucobound(foo,dim=i) /= 4) call abort()
  end subroutine five
end program test
