! Check we can cope with end labels in IO statements
program m
    implicit none
    integer i
    do while (.true.)
       read(*, *, end = 1) i
    end do
1   continue 
end program m
