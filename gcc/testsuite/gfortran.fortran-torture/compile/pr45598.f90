program main
implicit none
character(len=10) :: digit_string = '123456789'
character :: digit_arr(10)
call copy(digit_string, digit_arr)
print '(1x, a1)',digit_arr(1:9)
contains
  subroutine copy(in, out)
    character, dimension(10) :: in, out
    out(1:10) = in(1:10)
  end subroutine copy
end program main

