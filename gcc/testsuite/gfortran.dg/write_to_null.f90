! { dg-do run }
! pr18983
! could not write to /dev/null
       integer i
       open(10,file="/dev/null")
       do i = 1,100
         write(10,*) "Hello, world"
       end do
       end
