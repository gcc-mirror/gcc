! { dg-options "-std=gnu" }
! { dg-do compile }

! Former ICE when simplifying asind, plus wrong function name in error message
real, parameter :: d = asind(1.1) ! { dg-error "Argument of ASIND at.*must be between -1 and 1" }
print *, d

end
