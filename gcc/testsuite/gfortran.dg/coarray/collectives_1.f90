! { dg-do run }
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max
  intrinsic co_min
  intrinsic co_sum
  call test_min
  call test_max
  call test_sum
contains
  subroutine test_max
    integer :: val
    val = this_image ()
    call co_max (val, result_image=1)
    if (this_image() == 1) then
      !write(*,*) "Maximal value", val
      if (val /= num_images()) STOP 1
    end if
  end subroutine test_max

  subroutine test_min
    integer :: val
    val = this_image ()
    call co_min (val, result_image=1)
    if (this_image() == 1) then
      !write(*,*) "Minimal value", val
      if (val /= 1) STOP 2
    end if
  end subroutine test_min

  subroutine test_sum
    integer :: val, n
    val = this_image ()
    call co_sum (val, result_image=1)
    if (this_image() == 1) then
      !write(*,*) "The sum is ", val
      n = num_images()
      if (val /= (n**2 + n)/2) STOP 3
    end if
  end subroutine test_sum
end program test
