! { dg-do run }
!
! Check the fix for PR88143, in which the associate name caused
! a segfault in resolve.c. Make sure that the associate construct
! does its job correctly, as well as compiles.
!
! Contributed by Andrew Wood  <andrew@fluidgravity.co.uk>
!
MODULE m
   IMPLICIT NONE
   TYPE t
      INTEGER, DIMENSION(:), ALLOCATABLE :: i
   END TYPE
   CONTAINS
      SUBROUTINE s(x, idx1, idx2, k)
         CLASS(*), DIMENSION(:), INTENT(IN), OPTIONAL :: x
         INTEGER :: idx1, idx2, k
         SELECT TYPE ( x )
         CLASS IS ( t )
            ASSOCIATE ( j => x(idx1)%i )
               k = j(idx2)
            END ASSOCIATE
         END SELECT
      END
END

  use m
  class (t), allocatable :: c(:)
  integer :: k
  allocate (c(2))
  allocate (c(1)%i, source = [3,2,1])
  allocate (c(2)%i, source = [6,5,4])
  call s(c, 1, 3, k)
  if (k .ne. 1) stop 1
  call s(c, 2, 1, k)
  if (k .ne. 6) stop 2
end
