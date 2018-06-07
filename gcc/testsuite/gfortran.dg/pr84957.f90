! { dg-do compile }
! PR 84957
!
! Testcase derived from PR by G. Steinmetz  <gscfq@t-online.de>
!
function f() result(u)
  entry g() result(v)
contains
  function v(x) result(z)
    character :: x(2)
    character(sum(len_trim(x))) :: z
  end function v
  function u(x) result(z)
    character :: x(2)
    character(sum(len_trim(x))) :: z
  end function u
end function f
