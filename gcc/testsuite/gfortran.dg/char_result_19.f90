! { dg-do preprocess }
! { dg-additional-options "-cpp" }
!
! Test the fix for PR86248
!
! Contributed by Bill Long  <longb@cray.com>
!
program test
  use test_module
  implicit none
  integer :: i
  character(:), allocatable :: chr
  do i = 0, 2
    chr = func_1 (i)
    select case (i)
      case (0)
        if (chr .ne. 'el0') stop i
      case (1)
        if (chr .ne. 'el11') stop i
      case (2)
        if (chr .ne. 'el2') stop i
    end select
  end do
end program test
