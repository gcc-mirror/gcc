! { dg-do compile }
!
! F2003: 16.2.1
! "A name that identifies a common block in a scoping unit shall not be used 
!  to identify a constant or an intrinsic procedure in that scoping unit."
!
subroutine x134
 INTEGER, PARAMETER ::  C1=1  ! { dg-error "COMMON block 'c1' at \\(1\\) is used as PARAMETER" } 
 COMMON /C1/ I  ! { dg-error "COMMON block 'c1' at \\(1\\) is used as PARAMETER" } 
end subroutine
end
