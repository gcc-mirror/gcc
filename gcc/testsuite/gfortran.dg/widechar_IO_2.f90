! { dg-do run }
! Wide chracter I/O test 2, formatted array write and read
! Test case developed by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program chkdata
    integer, parameter :: k4=4
    character(len=7, kind=k4), dimension(3) :: mychar
    character(50) :: buffer
    mychar(1) = k4_"abc1234"
    mychar(2) = k4_"def5678"
    mychar(3) = k4_"ghi9012"
    buffer = ""
    write(buffer,'(3(a))') mychar(2:3), mychar(1)
    if (buffer /= "def5678ghi9012abc1234") STOP 1
    write(buffer,'(3(a))') mychar
    if (buffer /= "abc1234def5678ghi9012") STOP 2
    mychar = ""
    read(buffer,'(3(a))') mychar
    if (any(mychar.ne.[ k4_"abc1234",k4_"def5678",k4_"ghi9012" ])) STOP 3
end program chkdata
