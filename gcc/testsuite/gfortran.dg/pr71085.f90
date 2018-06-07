! { dg-do compile }
! PR 71085
!
! Testcase from PR by Vladimir Fuka <vladimir.fuka@gmail.com>
!
program pr71085
  print *, f()
contains
  function f()
    integer :: f(iargc()*10)
  end
end
