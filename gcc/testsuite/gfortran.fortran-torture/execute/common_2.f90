! PR fortran/16336 -- the two common blocks used to clash
MODULE bar
INTEGER :: I
COMMON /X/I
END MODULE bar

USE bar
INTEGER :: J
COMMON /X/J
j = 1
i = 2
if (j.ne.i) call abort()
if (j.ne.2) call abort()
END
