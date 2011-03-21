! { dg-do compile }
!
! PR fortran/33117, PR fortran/46478
! Procedures of a generic interface must be either
! all SUBROUTINEs or all FUNCTIONs.
!

!
! PR fortran/33117
!
module m1
  interface gen
    subroutine sub()            ! { dg-error "all SUBROUTINEs or all FUNCTIONs" }
    end subroutine sub
    function bar()
      real :: bar
    end function bar
  end interface gen
end module

!
! PR fortran/46478
!
MODULE m2
  INTERFACE new_name
    MODULE PROCEDURE func_name
    MODULE PROCEDURE subr_name
  END INTERFACE
CONTAINS
   LOGICAL FUNCTION func_name()  ! { dg-error "all SUBROUTINEs or all FUNCTIONs" }
   END FUNCTION
   SUBROUTINE subr_name()
   END SUBROUTINE
END MODULE

! { dg-final { cleanup-modules "m1 m2" } }
