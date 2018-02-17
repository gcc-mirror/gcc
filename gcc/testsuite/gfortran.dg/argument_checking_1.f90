! { dg-do run }
!
! PR fortran/30940
program main
  implicit none
  character(len=10) :: digit_string = '123456789', str
  character :: digit_arr(10)
  call copy(digit_string, digit_arr)
  call copy(digit_arr,str)
  if(str /= '123456789') STOP 1
  digit_string = 'qwertasdf'
  call copy2(digit_string, digit_arr)
  call copy2(digit_arr,str)
  if(str /= 'qwertasdf') STOP 2
  digit_string = '1qayxsw23e'
  call copy3("1qayxsw23e", digit_arr)
  call copy3(digit_arr,str)
  if(str /= '1qayxsw23e') STOP 3
contains
  subroutine copy(in, out)
    character, dimension(*)  :: in
    character, dimension(10) :: out
    out = in(:10)
  end subroutine copy
  subroutine copy2(in, out)
    character, dimension(2,*)  :: in
    character, dimension(2,5) :: out
    out(1:2,1:5) = in(1:2,1:5)
  end subroutine copy2
  subroutine copy3(in, out)
    character(len=2), dimension(5)  :: in
    character(len=2), dimension(5) :: out
    out = in
  end subroutine copy3
end program main
