! { dg-do compile }

! PR fortran/45474
! Definability checks for INTENT([IN]OUT) and intrinsics.

! Contributed by Tobias Burnus, burnus@gcc.gnu.org.

call execute_command_line("date", .true.,(1),1,'aa') ! { dg-error "variable definition context" }
call execute_command_line("date", .true.,1,(1),'aa') ! { dg-error "variable definition context" }
call execute_command_line("date", .true.,1,1,('aa')) ! { dg-error "variable definition context" }
end
