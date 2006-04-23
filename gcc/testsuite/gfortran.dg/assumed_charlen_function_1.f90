! { dg-do compile }
! { dg-options "-std=legacy" }
! Tests the patch for PRs 25084, 20852, 25085 and 25086, all of
! which involve assumed character length functions.
! Compiled from original PR testcases, which were all contributed
! by Joost VandeVondele  <jv244@cam.ac.uk>
!
! PR25084 - the error is not here but in any use of .IN.
! It is OK to define an assumed character length function
! in an interface but it cannot be invoked (5.1.1.5).

MODULE M1
 TYPE  SET
  INTEGER  CARD
 END  TYPE  SET
END MODULE M1

MODULE  INTEGER_SETS
 INTERFACE  OPERATOR  (.IN.)
  FUNCTION ELEMENT(X,A) ! { dg-error "cannot be assumed character length" }
     USE M1
     CHARACTER(LEN=*)      :: ELEMENT
     INTEGER, INTENT(IN)   ::  X
     TYPE(SET), INTENT(IN) ::   A
  END FUNCTION ELEMENT
 END  INTERFACE
END MODULE

! 5.1.1.5 of the Standard: A function name declared with an asterisk
! char-len-param shall not be array-valued, pointer-valued, recursive
! or pure
! 
! PR20852
RECURSIVE FUNCTION TEST() ! { dg-error "cannot be recursive" }
 CHARACTER(LEN=*) :: TEST
 TEST = ""
END FUNCTION

!PR25085
FUNCTION F1()             ! { dg-error "cannot be array-valued" }
  CHARACTER(LEN=*), DIMENSION(10) :: F1
  F1 = ""
END FUNCTION F1

!PR25086
FUNCTION F2() result(f4)  ! { dg-error "cannot be pointer-valued" }
  CHARACTER(LEN=*), POINTER  :: f4
  f4 = ""
END FUNCTION F2

!PR?????
pure FUNCTION F3()        ! { dg-error "cannot be pure" }
  CHARACTER(LEN=*)  :: F3
  F3 = ""
END FUNCTION F3

function not_OK (ch)
  character(*) not_OK, ch ! OK in an external function
  not_OK = ch
end function not_OK

  use m1

  character(4) :: answer
  character(*), external :: not_OK
  integer :: i
  type (set) :: z

  interface
    function ext (i)
      character(*) :: ext
      integer :: i
    end function ext
  end interface

  answer = not_OK ("unOK") ! { dg-error "since it is not a dummy" }

END

! { dg-final { cleanup-modules "M1" } }
