! { dg-do run }
!
! From posting by Spectrum to clf on thread entitled "Bounds for array pointer dummy argument".
!
PROGRAM X
    implicit none
    TYPE T
        INTEGER :: I
    END TYPE T
    TYPE(T), TARGET :: T1( 0:3 )

    associate( P => T1 % I )
        call check (lbound (P, 1), ubound (P, 1) ,1 , 4)
    endassociate

    associate( P2 => T1(:) % I )
        call check (lbound (P2, 1), ubound (P2, 1) ,1 , 4)
    endassociate

    associate( Q => T1 )
        call check (lbound (Q, 1), ubound (Q, 1) ,0 , 3)
    endassociate

    associate( Q2 => T1(:) )
        call check (lbound (Q2, 1), ubound (Q2, 1) ,1 , 4)
    endassociate
contains
    subroutine check (lbnd, ubnd, lower, upper)
      integer :: lbnd, ubnd, lower, upper
      if (lbnd .ne. lower) call abort
      if (ubnd .ne. upper) call abort
    end subroutine
END PROGRAM X
