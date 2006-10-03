! { dg-do compile }
! PR 19260  Test line continuations and spaces.
! Submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>.
x = si&  ! { dg-error "Unclassifiable statement" }
n(3.14159/2)
end
