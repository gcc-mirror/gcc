! { dg-do run }
! { dg-additional-sources bind_c_usage_33_c.c }
module m1
   implicit none
   contains
   subroutine odopen(unit)
      integer,intent(out) :: unit
      unit=8
   end subroutine
end module

module m2
   use iso_c_binding
   use m1
   implicit none
   contains
   subroutine c_odopen(unit) bind(c,name="odopen")
      integer(c_int),intent(out) :: unit
      call odopen(unit)
   end subroutine
end module
