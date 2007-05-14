! { dg-do compile }
! { dg-options "-std=f95" }
! PR30014 IOLENGTH does not handle KIND=8.  This patch checks the constraints.
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
! F95 Standard 9.6, R923
integer (kind=4) small, x
integer (kind=8) large
inquire (iolength=small) x
inquire (iolength=large) x ! { dg-error "requires default INTEGER" }
end
