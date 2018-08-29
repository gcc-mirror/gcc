! { dg-do run }
! PR32928 DATA statement with array element as initializer is rejected
! Test case by Jerry DeLisle  <jvdelisle @gcc.gnu.org>
program chkdata
    integer, parameter,dimension(4) :: myint = [ 4,3,2,1 ]
    character(3), parameter, dimension(3) :: mychar = [ "abc", "def", "ghi" ]
    character(50) :: buffer
    integer :: a(5)
    character(5) :: c(5)
    data a(1:2) / myint(4), myint(2) /
    data a(3:5) / myint(1), myint(3), myint(1) /
    data c / mychar(1), mychar(2), mychar(3), mychar(1), mychar(2) /
    buffer = ""
    if (any(a.ne.[1,3,4,2,4])) STOP 1
    write(buffer,'(5(a))')c
    if (buffer.ne."abc  def  ghi  abc  def  ") STOP 2
end program chkdata
