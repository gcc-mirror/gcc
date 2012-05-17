! { dg-do run }
! Tests the fix for the bug PR40629, in which the reference to 'x'
! in 'upper' wrongly host-associated with the symbol 'x' at module
! leve rather than the function.
!
! Contributed by Philippe Marguinaud  <philippe.marguinaud@meteo.fr>
!
MODULE m
  REAL :: x = 0
CONTAINS
  subroutine s
    call upper
    call lower
  CONTAINS
    SUBROUTINE upper
     y = x(3,1)
     if (int(y) .ne. 3) call abort
    END SUBROUTINE
    FUNCTION x(n, m)
       x = m*n
    END FUNCTION
    SUBROUTINE lower
     y = x(2,1)
     if (int(y) .ne. 2) call abort
    END SUBROUTINE
  END SUBROUTINE
END MODULE

  use m
  call s
end
