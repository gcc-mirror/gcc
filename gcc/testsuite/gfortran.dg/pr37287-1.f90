! PR debug/37287
! { dg-do link }
! { dg-options "-g -DPR37287_1" }
! { dg-additional-sources pr37287-2.F90 }
module pr37287_1
  use iso_c_binding, only : c_ptr, c_associated, c_null_ptr
  implicit none
contains
  subroutine set_null(ptr)
      type(c_ptr), intent(out) :: ptr
      ptr = c_null_ptr
  end subroutine set_null
end module pr37287_1
end
