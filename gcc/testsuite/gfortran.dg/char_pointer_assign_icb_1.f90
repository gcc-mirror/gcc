! { dg-do compile }
! Reduced testcase from PR 50892, regressed due to r256284 (PR 78534)
subroutine test
  use, intrinsic :: ISO_C_Binding, only: c_ptr
  type(c_ptr) :: text
  character(len=:), pointer :: ftext
  ftext => FortranChar(text)
contains
  function FortranChar ( C )
    type(c_ptr), intent(in), value :: C
    character(len=10), pointer :: FortranChar
  end function FortranChar
end subroutine test
