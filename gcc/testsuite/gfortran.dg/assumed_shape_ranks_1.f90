! { dg-do compile }
! Tests fix for PR25070; was no error for actual and assumed shape
! dummy ranks not matching.
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>

module addon
  interface extra
    function foo (y)
      integer :: foo (2), y (:)
    end function foo
  end interface extra
end module addon

  use addon
  INTEGER :: I(2,2)
  I=RESHAPE((/1,2,3,4/),(/2,2/))
  CALL TST(I)   ! { dg-error "Rank mismatch in argument" }
  i = foo (i)   ! { dg-error "Rank mismatch|Incompatible ranks" }
CONTAINS
  SUBROUTINE TST(I)
    INTEGER :: I(:)
    write(6,*) I
  END SUBROUTINE TST
END
