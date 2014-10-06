! { dg-do compile }
! { dg-options "" }
!
! Support Fortran 2015's IMPLICIT NONE with spec list
!

subroutine sub1
implicit none (type)
call test()
i = 1 ! { dg-error "Symbol 'i' at .1. has no IMPLICIT type" }
end subroutine sub1

subroutine sub2
implicit none ( external )
call foo() ! { dg-error "Procedure 'foo' called at .1. is not explicitly declared" }
i = 2
end subroutine sub2

subroutine sub3
implicit none ( external, type, external, type )
call foo() ! { dg-error "Procedure 'foo' called at .1. is not explicitly declared" }
i = 3 ! { dg-error "Symbol 'i' at .1. has no IMPLICIT type" }
end subroutine sub3

subroutine sub4
implicit none ( external ,type)
external foo
call foo()
i = 4 ! { dg-error "Symbol 'i' at .1. has no IMPLICIT type" }
end subroutine sub4

subroutine sub5  ! OK
implicit integer(a-z)
implicit none ( external )
procedure() :: foo
call foo()
i = 5
end subroutine sub5

subroutine sub6  ! OK
implicit none ( external )
implicit integer(a-z)
procedure() :: foo
call foo()
i = 5
end subroutine sub6

subroutine sub7
implicit none ( external )
implicit none ! { dg-error "Duplicate IMPLICIT NONE statement" }
end subroutine sub7

subroutine sub8
implicit none
implicit none ( type ) ! { dg-error "Duplicate IMPLICIT NONE statement" }
end subroutine sub8

subroutine sub9
implicit none ( external, type )
implicit integer(a-z) ! { dg-error "IMPLICIT statement at .1. following an IMPLICIT NONE .type. statement" }
procedure() :: foo
call foo()
end subroutine sub9

subroutine sub10
implicit integer(a-z)
implicit none ( external, type ) ! { dg-error "IMPLICIT NONE .type. statement at .1. following an IMPLICIT statement" }
procedure() :: foo
call foo()
end subroutine sub10
