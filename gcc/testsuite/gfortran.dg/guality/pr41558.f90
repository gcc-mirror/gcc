! PR debug/41558
! { dg-do run }
! { dg-skip-if "PR testsuite/51875" { { hppa*-*-hpux* } && { ! lp64 } } { "*" } { "" } }
! { dg-options "-g" }

subroutine f (s)
  character(len=3) :: s
  write (*,*) s ! { dg-final { gdb-test 7 "s" "'foo'" } }
end
  call f ('foo')
end
