! { dg-do compile }
! This tests the fix for PR29565, which failed in the gimplifier
! with the third call to has_read_key because this lost the first
! temporary array declaration from the current context.
!
! Contributed by William Mitchell  <william.mitchell@nist.gov>
!
  type element_t
    integer :: gid
  end type element_t

  type(element_t) :: element(1)
   call hash_read_key(element%gid)
   call hash_read_key(element%gid)
   call hash_read_key(element%gid)
contains
  subroutine hash_read_key(key)
    integer, intent(out) :: key(1)
  end subroutine hash_read_key
end
