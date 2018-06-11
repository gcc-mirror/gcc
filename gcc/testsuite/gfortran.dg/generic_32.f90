! { dg-do compile }
!
! PR 45521: [F08] GENERIC resolution with ALLOCATABLE/POINTER and PROCEDURE
!
! Contributed by Janus Weil <janus@gcc.gnu.org>


  INTERFACE gen
    SUBROUTINE suba(a)   ! { dg-error "Ambiguous interfaces" }
      REAL,ALLOCATABLE :: a(:)
    END SUBROUTINE
    SUBROUTINE subp(p)   ! { dg-error "Ambiguous interfaces" }
      REAL,POINTER,INTENT(IN) :: p(:)
    END SUBROUTINE
  END INTERFACE
end
