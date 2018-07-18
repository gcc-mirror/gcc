! { dg-do run }
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max
  intrinsic co_min
  intrinsic co_sum
  integer :: val(3), tmp_val(3)
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
  tmp_val = val
  call test_min
  val = tmp_val
  call test_max
  val = tmp_val
  call test_sum
contains
  subroutine test_max
    integer :: tmp
    call co_max (val(::2))
    if (num_images() > 1) then
      if (any (val /= [42, this_image(), 101])) STOP 1
    else
      if (any (val /= [42, this_image(), -55])) STOP 2
    endif

    val = tmp_val
    call co_max (val(:))
    if (num_images() > 1) then
      if (any (val /= [42, num_images(), 101])) STOP 3
    else
      if (any (val /= [42, num_images(), -55])) STOP 4
    endif
  end subroutine test_max

  subroutine test_min
    call co_min (val, result_image=num_images())
    if (this_image() == num_images()) then
      !write(*,*) "Minimal value", val
      if (num_images() > 1) then
        if (any (val /= [-99, 1, -55])) STOP 5
      else
        if (any (val /= [42, 1, -55])) STOP 6
      endif
    else
      if (any (val /= tmp_val)) STOP 7
    endif
  end subroutine test_min

  subroutine test_sum
    integer :: n
    n = 88
    call co_sum (val, result_image=1, stat=n)
    if (n /= 0) STOP 8
    if (this_image() == 1) then
      n = num_images()
      !write(*,*) "The sum is ", val
      if (any (val /= [42 + (n-1)*(-99), (n**2 + n)/2, -55+(n-1)*101])) STOP 9
    else
      if (any (val /= tmp_val)) STOP 10
    end if
  end subroutine test_sum
end program test
