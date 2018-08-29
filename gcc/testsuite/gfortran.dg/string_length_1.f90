! { dg-do run }
! Testcase for PR 31203
! We used to create strings with negative length
subroutine foo(i)
  integer :: i
  character(len=i) :: s(2)
  if (len(s) < 0) STOP 1
  if (len(s) /= max(i,0)) STOP 2
end

function gee(i)
  integer, intent(in) :: i
  character(len=i) :: gee

  gee = ""
end function gee

subroutine s1(i,j)
  character(len=i-j) :: a
  if (len(a) < 0) STOP 1
end subroutine

program test
  interface
    function gee(i)
      integer, intent(in) :: i
      character(len=i) :: gee
    end function gee
  end interface

  call foo(2)
  call foo(-1)
  call s1(1,2)
  call s1(-1,-8)
  call s1(-8,-1)

  if (len(gee(2)) /= 2) STOP 3
  if (len(gee(-5)) /= 0) STOP 4
  if (len(gee(intfunc(3))) /= max(intfunc(3),0)) STOP 5
  if (len(gee(intfunc(2))) /= max(intfunc(2),0)) STOP 6

  if (len(bar(2)) /= 2) STOP 7
  if (len(bar(-5)) /= 0) STOP 8
  if (len(bar(intfunc(3))) /= max(intfunc(3),0)) STOP 9
  if (len(bar(intfunc(2))) /= max(intfunc(2),0)) STOP 10

  if (cow(bar(2)) /= 2) STOP 11
  if (cow(bar(-5)) /= 0) STOP 12
  if (cow(bar(intfunc(3))) /= max(intfunc(3),0)) STOP 13
  if (cow(bar(intfunc(2))) /= max(intfunc(2),0)) STOP 14

contains

  function bar(i)
    integer, intent(in) :: i
    character(len=i) :: bar
  
    bar = ""
  end function bar

  function cow(c)
    character(len=*), intent(in) :: c
    integer :: cow
    cow = len(c)
  end function cow

  pure function intfunc(i)
    integer, intent(in) :: i
    integer :: intfunc

    intfunc = 2*i-5
  end function intfunc

end program test
