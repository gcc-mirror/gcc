! { dg-do compile }
!
! Tests the fix for PR69566, in which a boolean expression testing a
! the component of a pointer did not check the pointer, resulting in
! the ICE.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
  print *, associated(return_pointer()) ! ICE
contains
  function return_pointer()
    class(*), pointer :: return_pointer(:)
  end function
end
