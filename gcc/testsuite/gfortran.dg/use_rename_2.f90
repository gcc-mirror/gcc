! { dg-do run }
! { dg-options "-O1" }
! Checks the fix for PR34896 which was a regression that prevented max
! and min from being interchanged by the USE statement below.  It is further
! checked by libgomp/testsuite/libgomp.fortran/reduction5.f90
!
! Reported by H.J. Lu <hjl.tools@gmail.com>
!
module reduction5
  intrinsic min, max
end module reduction5

program reduction_5_regression
  call test2
contains
  subroutine test2
    use reduction5, min => max, max => min
    integer a, b
    a = max (1,5)
    b = min (1,5)
    if (a .ne. 1) call abort ()
    if (b .ne. 5) call abort ()
  end subroutine test2
end
