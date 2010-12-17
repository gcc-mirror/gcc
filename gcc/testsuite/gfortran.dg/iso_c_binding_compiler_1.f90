! { dg-do link }
!
! PR fortran/40569
!
! Check compiler_version/compiler_options intrinsics
!
subroutine test()
  use iso_fortran_env, only: compiler_version
  print '(3a)', '>>',compiler_version(),'<<'
end

use iso_fortran_env, foo => compiler_version, bar => compiler_version
  implicit none
  print *, foo()
  print *, bar()
  print '(3a)', '>',compiler_options(),'<'
  call test()
end
