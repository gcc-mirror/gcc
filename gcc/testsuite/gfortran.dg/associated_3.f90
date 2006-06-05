! { dg-do compile }
! Test for fix of PR27655
!
!Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org> 
  integer, pointer :: i
  print *, associated(NULL(),i) ! { dg-error "not permitted as actual argument" }
  print *, associated(i,NULL()) ! { dg-error "not permitted as actual argument" }
end
