! Check initialization of character variables via the DATA statement
CHARACTER*4 a
CHARACTER*6 b
CHARACTER*2 c
CHARACTER*4 d(2)
CHARACTER*4 e

DATA a(1:2) /'aa'/
DATA a(3:4) /'b'/
DATA b(2:6), c /'AAA', '12345'/
DATA d /2*'1234'/
DATA e(4:4), e(1:3) /'45', '123A'/

IF (a.NE.'aab ') STOP 1
IF (b.NE.' AAA   ') STOP 2
IF (c.NE.'12') STOP 3
IF (d(1).NE.d(2) .OR. d(1).NE.'1234') STOP 4
IF (e.NE.'1234') STOP 5
END
