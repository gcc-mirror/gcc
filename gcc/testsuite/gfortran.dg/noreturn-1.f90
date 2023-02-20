! Check for various valid and erroneous "noreturn" cases.
! { dg-do compile }
! { dg-options "-O2" }

module barbar
!GCC$ ATTRIBUTES noreturn :: bar1
contains
subroutine bar1
end subroutine bar1 ! { dg-warning "'noreturn' function does return" "detect falling off end of noreturn" }
end module

subroutine foo1
!GCC$ ATTRIBUTES noreturn :: foo1
end subroutine foo1 ! { dg-warning "'noreturn' function does return" "detect falling off end of noreturn" }

subroutine foo2
!GCC$ ATTRIBUTES noreturn :: foo2
call exit(0)
end subroutine foo2 ! { dg-bogus "warning:" "this function should not get any warnings" }

subroutine foo3
end subroutine foo3 ! { dg-bogus "warning:" "this function should not get any warnings" }

subroutine foo4
!GCC$ ATTRIBUTES noreturn :: foo4
call foo2()
end subroutine foo4 ! { dg-bogus "warning:" "this function should not get any warnings" }

subroutine foo5
!GCC$ ATTRIBUTES noreturn :: foo5
return              ! { dg-warning "'noreturn' function does return" "detect invalid return" }
end subroutine foo5

subroutine foo6
return
end subroutine foo6 ! { dg-bogus "warning:" "this function should not get any warnings" }

subroutine foo7
call foo6()
end subroutine foo7 ! { dg-bogus "warning:" "this function should not get any warnings" }

subroutine foo8
!GCC$ ATTRIBUTES noreturn :: foo8
call foo7()
end subroutine foo8 ! { dg-warning "'noreturn' function does return" "detect return from tail call" }

subroutine foo9
!GCC$ ATTRIBUTES noreturn :: foo9
interface
subroutine bar
!GCC$ ATTRIBUTES noreturn :: bar
end subroutine bar
end interface
call bar()
end subroutine foo9 ! { dg-bogus "warning:" "this function should not get any warnings" }

function ffo1()
implicit none
!GCC$ ATTRIBUTES noreturn :: ffo1
integer :: ffo1
ffo1 = 0
end function ffo1   ! { dg-warning "'noreturn' function does return" "detect falling off end of noreturn" }
