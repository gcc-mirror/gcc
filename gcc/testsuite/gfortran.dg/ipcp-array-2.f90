! { dg-do compile }
! { dg-options "-O3 -fno-inline -fwhole-program -fdump-ipa-cp-details -fdump-tree-lversion-details" }

module x
  implicit none
contains
  subroutine foo(a, b)
    real :: a(:,:)
    real :: b
    integer :: i,j
    b = 0.
    do j=1,size(a,2)
       do i=1,size(a,1)
          b = b + a(i,j) * i * j
       end do
    end do
  end subroutine foo

  subroutine bar(a, b)
    real :: a(:,:)
    real :: b
    call foo (a,b)
  end subroutine bar

end module x

program main
  use x
  implicit none
  integer :: n, m
  real, dimension(4,3) :: a
  real, dimension(3,4) :: c     
  real :: b
  call random_number(a)
  call bar(a,b)
  print *,b
  
  call random_number(c)
  call bar(c,b)
  print *,b
  
end program main

! { dg-final { scan-ipa-dump "op assert_expr 1" "cp" } }
! { dg-final { scan-tree-dump-not "versioned this loop for when certain strides are 1" "lversion" } }
