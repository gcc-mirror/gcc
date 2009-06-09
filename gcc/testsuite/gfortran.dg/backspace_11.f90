! { dg-do run }
! PR 40334 backspace regression
program backspace_11
 implicit none
 character(len=5) :: str
 open(10, access='sequential', status='scratch')
 write(10,'(A)')'HELLO'
 rewind(10)

 do
    read(10,'(A)',end=1) str
 enddo
1 backspace 10
 !the file pointer is now at EOF

 read(10,*,end=2) str
 call abort
2 backspace 10
 !the file pointer is now at EOF

 read(10,'(A)',end=3) str
 call abort
3 continue
end program backspace_11
