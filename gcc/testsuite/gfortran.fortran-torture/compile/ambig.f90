MODULE TYPESP
  TYPE DMT
     REAL(KIND(1.D0)), POINTER :: ASPK(:)
  END TYPE DMT
END MODULE TYPESP

MODULE TCNST
  Integer, Parameter :: DIM_TEMP_BUFFER=10000  
  Real(Kind(1.d0)), Parameter :: COLROW_=0.33,PERCENT=0.7    
end MODULE TCNST


Subroutine DOWORK(A)
  Use TYPESP
  Use TCNST
  Type(DMT), intent (inout)     :: A
  Real(Kind(1.d0)),Pointer      ::  ASPK(:)
  Integer                       ::  ISIZE, IDIM

  ISIZE=DIM_TEMP_BUFFER
  
  Allocate(ASPK(ISIZE),STAT=INFO)
  IDIM = MIN(ISIZE,SIZE(A%ASPK))
  ASPK(1:IDIM)  = A%ASPK(1:IDIM)
  Return
End Subroutine DOWORK
