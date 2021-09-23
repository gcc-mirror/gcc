! { dg-do compile }
!
! PR fortran/99651
!
module m
    type :: CHAR_STAR
      character(len=1),dimension(:),pointer :: ptr
    end type
    type(CHAR_STAR), parameter ::CHAR_STAR_NULL = CHAR_STAR(NULL())
end module m

use m
type typeNode
    type(typeNode),   pointer       :: Next => null()
end type typeNode
end
