! { dg-do run }
!
! PR 125762
!
! A DATA statement initializing a pointer to an array element (or array-element
! substring) target segfaulted in gfc_conv_scalarized_array_ref: the element
! reference reached gfc_conv_array_ref with an unresolved type (AR_UNKNOWN) and
! no scalarizer, so the scalarized path was wrongly taken.

program p
  integer, target :: t(10) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  integer, pointer :: p1, p2

  character(5), target :: s(2) = ["abcde", "fghij"]
  character(2), pointer :: cp

  data p1 /t(3)/
  data p2 /t(7)/
  data cp /s(2)(2:3)/

  if (p1 /= 3) stop 1
  if (.not. associated (p1, t(3))) stop 2
  if (p2 /= 7) stop 3
  if (cp /= "gh") stop 4
end program p
