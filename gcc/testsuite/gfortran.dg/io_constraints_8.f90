! { dg-do compile }
! { dg-options "-fmax-errors=100 -Wall" }
!
! PR fortran/48972
!
!
! All string arguments to I/O statements shall
! be of default-character type. (Except for the
! internal unit.)
!

character(len=30, kind=4) :: str1
integer :: i

OPEN(99, access=4_'direct')     ! { dg-error "must be a character string of default kind" }
OPEN(99, action=4_'read')       ! { dg-error "must be a character string of default kind" }
OPEN(99, asynchronous=4_'no')   ! { dg-error "must be of default CHARACTER kind" }
OPEN(99, blank=4_'null')        ! { dg-error "must be a character string of default kind" }
OPEN(99, decimal=4_'comma')     ! { dg-error "must be a character string of default kind" }
OPEN(99, delim=4_'quote')       ! { dg-error "must be a character string of default kind" }
OPEN(99, encoding=4_'default')  ! { dg-error "must be a character string of default kind" }
OPEN(99, file=4_'Test.dat')     ! { dg-error "must be a character string of default kind" }
OPEN(99, form=4_'formatted')    ! { dg-error "must be a character string of default kind" }
OPEN(99, pad=4_'yes')           ! { dg-error "must be a character string of default kind" }
OPEN(99, position=4_'asis')     ! { dg-error "must be a character string of default kind" }
OPEN(99, round=4_'down')        ! { dg-error "must be a character string of default kind" }
OPEN(99, sign=4_'plus')         ! { dg-error "must be a character string of default kind" }
OPEN(99, status=4_'old')        ! { dg-error "must be a character string of default kind" }
OPEN(99, IOSTAT=i, iomsg=str1)  ! { dg-error "must be a character string of default kind" }

close(99, iostat=i, iomsg=str1) ! { dg-error "must be a character string of default kind" }
close(99, status=4_'delete')    ! { dg-error "must be a character string of default kind" }

write(99, '(a)', advance=4_'no')! { dg-error "must be a character string of default kind" }
read (99, *, blank=4_'null')    ! { dg-error "must be a character string of default kind" }
write(99, *, decimal=4_'comma') ! { dg-error "must be a character string of default kind" }
write(99, *, delim=4_'quote')   ! { dg-error "must be a character string of default kind" }
read (99, *, pad=4_'yes')       ! { dg-error "must be a character string of default kind" }
write(99, *, round=4_'down')    ! { dg-error "must be a character string of default kind" }
write(99, *, sign=4_'plus')     ! { dg-error "must be a character string of default kind" }

wait(99, iostat=i, iomsg=str1)  ! { dg-error "must be a character string of default kind" }

endfile  (99, iostat=i, iomsg=str1)  ! { dg-error "must be a character string of default kind" }
backspace(99, iostat=i, iomsg=str1)  ! { dg-error "must be a character string of default kind" }
rewind   (99, iostat=i, iomsg=str1)  ! { dg-error "must be a character string of default kind" }
flush    (99, iostat=i, iomsg=str1)  ! { dg-error "must be a character string of default kind" }

inquire (file=str1)               ! { dg-error "must be a character string of default kind" }
inquire (99,access=str1)          ! { dg-error "must be a character string of default kind" }
inquire (99,action=str1)          ! { dg-error "must be a character string of default kind" }
inquire (99,asynchronous=str1)    ! { dg-error "must be a character string of default kind" }
inquire (99,blank=str1)           ! { dg-error "must be a character string of default kind" }
inquire (99,decimal=str1)         ! { dg-error "must be a character string of default kind" }
inquire (99,delim=str1)           ! { dg-error "must be a character string of default kind" }
inquire (99,direct=str1)          ! { dg-error "must be a character string of default kind" }
inquire (99,encoding=str1)        ! { dg-error "must be a character string of default kind" }
inquire (99,form=str1)            ! { dg-error "must be a character string of default kind" }
inquire (99,formatted=str1)       ! { dg-error "must be a character string of default kind" }
inquire (99,iomsg=str1, iostat=i) ! { dg-error "must be a character string of default kind" }
inquire (99,name=str1)            ! { dg-error "must be a character string of default kind" }
inquire (99,pad=str1)             ! { dg-error "must be a character string of default kind" }
inquire (99,position=str1)        ! { dg-error "must be a character string of default kind" }
inquire (99,read=str1)            ! { dg-error "must be a character string of default kind" }
inquire (99,readwrite=str1)       ! { dg-error "must be a character string of default kind" }
inquire (99,round=str1)           ! { dg-error "must be a character string of default kind" }
inquire (99,sequential=str1)      ! { dg-error "must be a character string of default kind" }
inquire (99,sign=str1)            ! { dg-error "must be a character string of default kind" }
!inquire (99,stream=str1)  ! Fails due to PR 48976
inquire (99,unformatted=str1)     ! { dg-error "must be a character string of default kind" }
inquire (99,write=str1)           ! { dg-error "must be a character string of default kind" }
end
