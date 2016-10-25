! { dg-do compile }
! { dg-options "-fdec" }
!
! Test the usage of TYPE as an alias for PRINT.
!
! Note the heavy use of other TYPE statements to test for
! regressions involving ambiguity.
!
program main

logical bool
integer i /0/, j /1/, k /2/
character(*), parameter :: fmtstr = "(A11)"
namelist /nmlist/ i, j, k
integer, parameter :: n = 5
real a(n)

! derived type declarations
type is
  integer i
end type

type point
   real x, y
end type point

type, extends(point) :: point_3d
   real :: z
end type point_3d

type, extends(point) :: color_point
   integer :: color
end type color_point

! declaration type specification
type(is) x
type(point), target :: p
type(point_3d), target :: p3
type(color_point), target :: c
class(point), pointer :: p_or_c

! select type
p_or_c => c
select type ( a => p_or_c )
  class is ( point )
    print *, "point"     ! <===
  type is ( point_3d )
    print *, "point 3D"
end select

! Type as alias for print
type*
type *
type*,'St','ar'
type *, 'St', 'ar'
type 10, 'Integer literal'
type 10, 'Integer variable'
type '(A11)', 'Character literal'
type fmtstr, 'Character variable'
type nmlist ! namelist

a(1) = 0
call f(.true., a, n)

10    format (A11)

end program


subroutine f(b,a,n)
  implicit none
  logical b
  real a(*)
  integer n

  integer i

  do i = 2,n
    a(i) = 2 * (a(i-1) + 1)
    if (b) type*,a(i) ! test TYPE as PRINT inside one-line IF
  enddo

  return
end subroutine
