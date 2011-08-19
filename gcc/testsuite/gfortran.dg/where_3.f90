! { dg-do compile }
!
! PR fortran/50129
! ICE after reporting an error on a masked ELSEWHERE statement following an
! unmasked one.
!
! Contributed by Joost Van de Vondele <Joost.VandeVondele@pci.uzh.ch>

INTEGER :: I(3)
WHERE (I>2)
ELSEWHERE
ELSEWHERE (I<1) ! { dg-error "follows previous unmasked ELSEWHERE" }
END WHERE
END

