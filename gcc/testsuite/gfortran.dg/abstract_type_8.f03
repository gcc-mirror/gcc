! { dg-do compile }
!
! PR 44616: [OOP] ICE if CLASS(foo) is used before its definition
!
! Contributed by bd satish <bdsatish@gmail.com>

module factory_pattern
implicit none

type First_Factory
    character(len=20) :: factory_type
    class(Connection), pointer :: connection_type
    contains
end type First_Factory

type, abstract :: Connection
    contains
    procedure(generic_desc), deferred :: description
end type Connection

abstract interface
    subroutine generic_desc(self)
        import  ! Required, cf. PR 44614
        class(Connection) :: self
    end subroutine generic_desc
end interface
end module factory_pattern
