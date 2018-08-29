! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR83567 in which the parameterized component 'foo' was
! being deallocated before return from 'addw', with consequent segfault in 
! the main program.
!
! Contributed by Berke Durak  <berke.durak@gmail.com>
! The function 'addvv' has been made elemental so that the test can check that
! arrays are correctly treated and that no memory leaks occur.
!
module pdt_m
  implicit none
  type :: vec(k)
     integer, len :: k=3
     integer :: foo(k)=[1,2,3]
  end type vec
contains
  elemental function addvv(a,b) result(c)
    type(vec(k=*)), intent(in) :: a
    type(vec(k=*)), intent(in) :: b
    type(vec(k=a%k)) :: c

    c%foo=a%foo+b%foo
  end function
end module pdt_m

program test_pdt
  use pdt_m
  implicit none
  type(vec) :: u,v,w, a(2), b(2), c(2)
  integer :: i

  u%foo=[1,2,3]
  v%foo=[2,3,4]
  w=addvv(u,v)
  if (any (w%foo .ne. [3,5,7])) STOP 1
  do i = 1 , a(1)%k
    a%foo(i) = i + 4
    b%foo(i) = i + 7
  end do
  c = addvv(a,b)
  if (any (c(1)%foo .ne. [13,15,17])) STOP 2
end program test_pdt
! { dg-final { scan-tree-dump-times "__builtin_free" 8 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_malloc" 9 "original" } }
