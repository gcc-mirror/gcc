! { dg-do compile }
! Test fix of PR24705 - ICE on assumed character length
! internal function.
!
character (6) :: c
  c = f1 ()
  if (c .ne. 'abcdef') call abort
contains
  function f1 () ! { dg-error "must not be assumed length" }
    character (*) :: f1
    f1 = 'abcdef'
  end function f1
end