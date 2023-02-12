! { dg-do compile }
! { dg-options "-O2 -fno-inline -fno-ipa-cp -fwhole-program -fdump-ipa-sra-details" }

module foo
  implicit none
contains
  subroutine bar(a,x)
    real, dimension(:,:), intent(in) :: a
    real, intent(out) :: x
    integer :: i,j

    x = 0
    do j=1,ubound(a,2)
       do i=1,ubound(a,1)
          x = x + a(i,j)**2
       end do
    end do
  end subroutine bar
end module foo

program main
  use foo
  implicit none
  real, dimension(2,3) :: a
  real :: x
  integer :: i

  data a /1.0, 2.0, 3.0, -1.0, -2.0, -3.0/

  do i=1,2000000
     call bar(a,x)
  end do
  print *,x
end program main

! { dg-final { scan-ipa-dump "Created new node.*bar\\.isra" "sra" } }
! { dg-final { scan-ipa-dump-times "IPA_PARAM_OP_SPLIT" 7 "sra" } }
