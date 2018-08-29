! PR fortran/16336 -- the two common blocks used to clash
MODULE bar
INTEGER :: I
COMMON /X/I
contains 
subroutine set_i()
i = 5
end subroutine set_i
END MODULE bar

USE bar
INTEGER :: J
COMMON /X/J
j = 1
i = 2
if (j.ne.i) STOP 1
if (j.ne.2) STOP 2
call set_i()
if (j.ne.5) STOP 3
END
