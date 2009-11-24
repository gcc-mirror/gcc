! { dg-do compile }
!
! PR42045: [F03] passing a procedure pointer component to a procedure pointer dummy
!
! Contributed by John McFarland <john.mcfarland@swri.org>

PROGRAM prog
 TYPE object
  PROCEDURE(), POINTER, NOPASS :: f
 END TYPE object
 TYPE container
  TYPE (object), POINTER :: o(:)
 END TYPE container
 TYPE (container) :: c
 TYPE (object) :: o1, o2
 PROCEDURE(), POINTER :: f => NULL()
 o1%f => f
 CALL set_func(o2,f)
 CALL set_func(o2,o1%f)
 ALLOCATE( c%o(5) )
 c%o(5)%f => f
 CALL set_func(o2,c%o(5)%f)
CONTAINS
 SUBROUTINE set_func(o,f)
  TYPE (object) :: o
  PROCEDURE(), POINTER :: f
  o%f => f
 END SUBROUTINE set_func
END PROGRAM prog
