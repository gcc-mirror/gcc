! { dg-do run }
! Tests the fix for PR27124 in which the unpacking of argument
! temporaries and of array result temporaries occurred in the
! incorrect order.
! 
! Test is based on the original example, provided by
! Philippe Schaffnit <P.Schaffnit@access.rwth-aachen.de>
!
  PROGRAM Test
    INTEGER :: Array(2, 3) = reshape ((/1,4,2,5,3,6/),(/2,3/))
    integer :: Brray(2, 3) = 0
    Brray(1,:) = Function_Test (Array(1,:))
    if (any(reshape (Brray, (/6/)) .ne. (/11, 0, 12, 0, 13, 0/))) STOP 1
    Array(1,:) = Function_Test (Array(1,:))
    if (any(reshape (Array, (/6/)) .ne. (/11, 4, 12, 5, 13, 6/))) STOP 2

  contains
      FUNCTION Function_Test (Input)
          INTEGER, INTENT(IN) :: Input(1:3)
          INTEGER :: Function_Test(1:3)
          Function_Test = Input + 10
      END FUNCTION Function_Test
  END PROGRAM Test

