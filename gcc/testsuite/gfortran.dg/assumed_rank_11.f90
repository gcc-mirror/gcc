! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/48820
!
! Assumed-rank tests
subroutine foo(X)
 integer :: x(..)
 codimension :: x[*] ! { dg-error "The assumed-rank array 'x' at .1. shall not have a codimension" }
end

subroutine foo2(X)
 integer, dimension(..) :: x[*] ! { dg-error "The assumed-rank array at .1. shall not have a codimension" }
end

subroutine foo3(X)
 integer, codimension[*] :: x(..) ! { dg-error "The assumed-rank array at .1. shall not have a codimension" }
end

subroutine foo4(X)
 integer, codimension[*], dimension(..) :: x ! { dg-error "The assumed-rank array at .1. shall not have a codimension" }
end

subroutine bar(X)
 integer :: x[*]
 dimension :: x(..) ! { dg-error "The assumed-rank array 'x' at .1. shall not have a codimension" }
end

subroutine foobar(X)
 integer :: x
 codimension :: x[*]
 dimension :: x(..) ! { dg-error "The assumed-rank array 'x' at .1. shall not have a codimension" }
end

subroutine barfoo(X)
 integer :: x
 dimension :: x(..)
 codimension :: x[*] ! { dg-error "The assumed-rank array 'x' at .1. shall not have a codimension" }
end

subroutine orig(X) ! { dg-error "may not have the VALUE or CODIMENSION attribute" }
 integer :: x(..)[*]
end

subroutine val1(X)
 integer, value :: x(..)  ! { dg-error "VALUE attribute conflicts with DIMENSION attribute" }
end

subroutine val2(X)
 integer, value :: x
 dimension :: x(..)  ! { dg-error "VALUE attribute conflicts with DIMENSION attribute" }
end
