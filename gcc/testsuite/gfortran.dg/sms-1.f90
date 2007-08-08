! { dg-do run }
! { dg-options "-O2 -fmodulo-sched" }
! This testcase related to INC instruction which is
! currently not supported in SMS. 
program main
  integer (kind = 8) :: i, l8, u8, step8
  integer (kind = 4) :: l4, step4
  integer (kind = 8), parameter :: big = 10000000000_8

  u8 = big * 40 + 200
  l4 = 200
  step8 = -big
  call test ((/ (i, i = u8, l4, step8) /), u8, l4 + 0_8, step8)
contains
  subroutine test (a, l, u, step)
    integer (kind = 8), dimension (:), intent (in) :: a
    integer (kind = 8), intent (in) :: l, u, step
    integer (kind = 8) :: i
    integer :: j

    j = 1
    do i = l, u, step
      if (a (j) .ne. i) call abort
      j = j + 1
    end do
    if (size (a, 1) .ne. j - 1) call abort
  end subroutine test
end program main


