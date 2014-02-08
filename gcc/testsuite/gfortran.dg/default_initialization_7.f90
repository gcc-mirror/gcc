! { dg-do compile }
!
! PR fortran/57033
! ICE on a structure constructor of an extended derived type whose parent
! type last component has a default initializer
!
! Contributed by Tilo Schwarz <tilo@tilo-schwarz.de>

program ice

type m
    integer i
    logical :: f = .false.
end type m

type, extends(m) :: me
end type me

type(me) meo

meo = me(1)              ! ICE
end program ice
