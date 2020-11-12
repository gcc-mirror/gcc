! { dg-do compile }
! { dg-additional-options "-fprofile-arcs -ftest-coverage" }
!
! PR fortran/95847
!
module foo
contains
    subroutine sbr()
    end subroutine sbr
end module foo

function foo_suite() result(suite)
   use foo
   integer :: bar
   integer :: res
   res = bar(sbr)
end function foo_suite
