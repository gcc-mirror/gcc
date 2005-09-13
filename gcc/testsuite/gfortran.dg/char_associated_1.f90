! Check that associated works correctly for character arrays.
! { dg-do run }
program main
  character (len = 5), dimension (:), pointer :: ptr
  character (len = 5), dimension (2), target :: a = (/ 'abcde', 'fghij' /)
  ptr => a
  if (.not. associated (ptr, a)) call abort
end program main
