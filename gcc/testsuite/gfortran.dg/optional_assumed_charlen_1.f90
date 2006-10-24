! { dg-do compile }
! Tests the fix for PR29284 in which an ICE would occur in converting
! the call to a suboutine with an assumed character length, optional
! dummy that is not present.
!
! Contributed by Rakuen Himawari  <rakuen_himawari@yahoo.co.jp>
!
      MODULE foo
      CONTAINS
        SUBROUTINE sub1(a)
          CHARACTER (LEN=*), OPTIONAL :: a
          WRITE(*,*) 'foo bar'
        END SUBROUTINE sub1

      SUBROUTINE sub2
        CALL sub1()
      END SUBROUTINE sub2

     END MODULE foo
! { dg-final { cleanup-modules "foo" } }
