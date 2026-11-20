! { dg-do compile }
! { dg-options "-fcheck=bounds" }
!
! PR128876
!
! CLASS dummy of a PDT type with -fcheck=bounds: ICE (gcc_unreachable) in
! gfc_trans_deferred_vars.  -fcheck=bounds defers the dummy's init, and
! the CLASS PDT branch there now requires !CLASS_DATA (sym)->attr.pointer,
! which is never true: _data of a class container always has attr.pointer;
! a CLASS POINTER entity is attr.class_pointer.
!
! Regression from r17 6335154826f, PDT (9/16) [PR82173]; works with 16.2.1.
! Found building fiats (example/learn-addition.F90, fpm debug profile).
!
! Contributed by Jerry Delisle  <jvdelisle@gcc.gnu.org>
!
module pdt_class_dummy_m
  implicit none
  type t(k)
    integer, kind :: k = kind(1.0)
    real(k) :: x
  end type
contains
  subroutine s (a)
    class(t), intent(in) :: a
    print *, a%x
  end subroutine
end module
