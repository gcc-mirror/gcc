! { dg-do run }
! { dg-options -Wsurprising }
integer, pointer :: p
integer, target :: t, s

! The tests for character pointers are currently commented out,
! because they don't yet work correctly.
! This is PR 17192
!!$character*5, pointer :: d
!!$character*5, target :: c, e

t = 1
p => s
! We didn't dereference the pointer in the following line.
p = f() ! { dg-warning "POINTER-valued function" }
p = p+1
if (p.ne.2) STOP 1
if (p.ne.s) STOP 2

!!$! verify that we also dereference correctly the result of a function 
!!$! which returns its result by reference
!!$c = "Hallo"
!!$d => e
!!$d = g() !  dg-warning "POINTER valued function" "" 
!!$if (d.ne."Hallo") STOP 3

contains
function f()
integer, pointer :: f
f => t
end function f
!!$function g()
!!$character, pointer :: g
!!$g => c
!!$end function g
end
