! { dg-do compile }
! Bug 45794 - ICE: Segmentation fault in gfc_conv_procedure_call
subroutine foo (vector, mask)
  real :: vector(:)
  logical, optional :: mask(:)
  integer :: loc(1)
  if (present(mask)) then
    loc = maxloc(vector, mask)
  end if
end subroutine
