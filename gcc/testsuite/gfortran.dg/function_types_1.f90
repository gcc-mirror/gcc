! { dg-do compile }
! Tests the fix for PR34431 in which function TYPEs that were
! USE associated would cause an error.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module bar
contains
  type(non_exist) function func2() ! { dg-error "not accessible" }
  end function func2
end module bar
