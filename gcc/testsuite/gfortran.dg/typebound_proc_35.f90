! { dg-do run }
!
! PR 78443: [OOP] Incorrect behavior with non_overridable keyword
!
! Contributed by federico <perini@wisc.edu>

module types
    implicit none


    ! Abstract parent class and its child type
    type, abstract :: P1
    contains
        procedure :: test => test1
        procedure (square_interface), deferred :: square
    endtype

    ! Deferred procedure interface
    abstract interface
        function square_interface( this, x ) result( y )
           import P1
           class(P1) :: this
           real :: x, y
        end function square_interface
    end interface

    type, extends(P1) :: C1
    contains
       procedure, non_overridable :: square => C1_square
    endtype

    ! Non-abstract parent class and its child type
    type :: P2
    contains
        procedure :: test => test2
        procedure :: square => P2_square
    endtype

    type, extends(P2) :: C2
    contains
       procedure, non_overridable :: square => C2_square
    endtype

contains

    real function test1( this, x )
        class(P1) :: this
        real :: x
        test1 = this % square( x )
    end function

    real function test2( this, x )
        class(P2) :: this
        real :: x
        test2 = this % square( x )
    end function

    function P2_square( this, x ) result( y )
       class(P2) :: this
       real :: x, y
       y = -100.      ! dummy
    end function

    function C1_square( this, x ) result( y )
       class(C1) :: this
       real :: x, y
       y = x**2
    end function

    function C2_square( this, x ) result( y )
       class(C2) :: this
       real :: x, y
       y = x**2
    end function

end module

program main
    use types
    implicit none
    type(P2) :: t1
    type(C2) :: t2
    type(C1) :: t3

    if ( t1 % test( 2. ) /= -100.) call abort()
    if ( t2 % test( 2. ) /= 4.) call abort()
    if ( t3 % test( 2. ) /= 4.) call abort()
end program
