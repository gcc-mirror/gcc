! { dg-do run { target { nonpic || pie_enabled } } }
! { dg-options "-O2" }

program bar
call foo1()
call noreturn_autodetection_failed() ! check if optimized out 
end program

subroutine foo1
stop 0
end subroutine foo1
