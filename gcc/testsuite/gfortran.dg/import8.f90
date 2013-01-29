! { dg-do compile }
!
! PR fortran/44614
!
!

implicit none

type, abstract :: Connection
end type Connection

abstract interface
    subroutine generic_desc(self)
        ! <<< missing IMPORT 
        class(Connection) :: self ! { dg-error "is being used before it is defined" }
    end subroutine generic_desc
end interface
end
