Module radin_mod
  INTEGER, PARAMETER :: DP = selected_real_kind(14,200)
Contains
  Subroutine SPLIFT (X,Y,YP,YPP,N,IERR,ISX,A1,B1,AN,BN)
    Integer,  Intent(in) :: N,ISX
    Real(dp), Intent(in) :: X(N),Y(N),A1,B1,AN,BN
    Real(dp), Intent(out) :: YP(N),YPP(N)
    Real(dp), Allocatable, Dimension(:,:) :: W
    NM1  = N-1
    NM2  = N-2
    If (ISX.Gt.0) GO TO 40
    Do I=2,N
       If (X(I)-X(I-1) .Le. 0) Then
          IERR = 3
          Return
       Endif
    End Do
    Allocate(W(N,3))
40  YPP(1) = 4*B1
    DOLD = (Y(2)-Y(1))/W(2,2)
    Do  I=2,NM2
       DNEW   = (Y(I+1) - Y(I))/W(I+1,2)
       YPP(I) = 6*(DNEW - DOLD)
       YP(I)  = DOLD
       DOLD = DNEW
    End Do
    Return
  End Subroutine SPLIFT
End Module radin_mod

