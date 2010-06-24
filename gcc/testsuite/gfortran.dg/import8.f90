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
        class(Connection) :: self ! { dg-error "has not been declared within the interface" }
    end subroutine generic_desc
end interface
end
