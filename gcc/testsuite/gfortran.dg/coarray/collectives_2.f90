! { dg-do run }
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max
  intrinsic co_min
  intrinsic co_sum
  integer :: val(3)
  integer :: vec(3)
  vec = [2,3,1]
  if (this_image() == 1) then
    val(1) = 42
  else
    val(1) = -99
  endif
  val(2) = this_image()
  if (this_image() == num_images()) then
    val(3) = -55
  else
    val(3) = 101
  endif
  call test_min
  call test_max
  call test_sum
contains
  subroutine test_max
    call co_max (val(vec))
    !write(*,*) "Maximal value", val
    if (num_images() > 1) then
      if (any (val /= [42, num_images(), 101])) call abort()
    else
      if (any (val /= [42, num_images(), -55])) call abort()
    endif
  end subroutine test_max

  subroutine test_min
    call co_min (val, result_image=num_images())
    if (this_image() == num_images()) then
      !write(*,*) "Minimal value", val
      if (num_images() > 1) then
        if (any (val /= [-99, num_images(), -55])) call abort()
      else
        if (any (val /= [42, num_images(), -55])) call abort()
      endif
    endif
  end subroutine test_min

  subroutine test_sum
    integer :: n
    call co_sum (val, result_image=1)
    if (this_image() == 1) then
      n = num_images()
      !write(*,*) "The sum is ", val
      if (any (val /= [42 + (n-1)*(-99), (n**2 + n)/2, -55+(n-1)*101])) call abort()
    end if
  end subroutine test_sum
end program test
