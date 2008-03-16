! { dg-do compile }
! Tests the fix for PR35470, in which the pointer assignment would fail
! because the assumed size 'arr' would get mixed up with the component
! 'p' in the check for the upper bound of an assumed size array.
!
! Contributed by Antony Lewis <antony@cosmologist.info>
!
subroutine sub(arr)
  type real_pointer
    real, pointer :: p(:)
  end type real_pointer
  type(real_pointer), dimension(*) :: arr
  real, pointer :: p(:)
  p => arr(1)%p
end subroutine
