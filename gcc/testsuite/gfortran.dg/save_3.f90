! { dg-do compile }

! PR fortran/35837
! We used do have a problem with resolving "save all" and nested namespaces.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module g95bug
save
integer :: i=20
contains
pure function tell_i() result (answer)
  integer :: answer
  answer=i
end function tell_i
end module g95bug

! { dg-final { cleanup-modules "g95bug" } }
