	CHARACTER*4 A(3),B(3),C(3)
	DATA A /'A',"A",'A'/
	DATA B /3*'A'/
	DATA C /'A', 2*'A'/
	IF (ANY(A.NE.B).OR.ANY(A.NE.C)) STOP 1
	END
