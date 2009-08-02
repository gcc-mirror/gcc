! { dg-do compile }
! { dg-options "-std=legacy" }
!
! Check that PR20877 and PR25047 are fixed by the patch for
! PR24558. Both modules would emit the error:
! insert_bbt(): Duplicate key found!
! because of the prior references to a module function entry.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
MODULE TT
CONTAINS
  FUNCTION K(I) RESULT(J)
    ENTRY J() ! { dg-error "conflicts with RESULT attribute" }
  END FUNCTION K

  integer function foo ()
    character*4 bar ! { dg-error "type CHARACTER" }
    foo = 21
    return
  entry bar ()
    bar = "abcd"
  end function
END MODULE TT


! { dg-final { cleanup-modules "TT" } }
