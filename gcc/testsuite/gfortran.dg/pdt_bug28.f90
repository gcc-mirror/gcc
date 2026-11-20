! { dg-do run }
!
! PR128876
!
! Type-bound call on a PDT component of a PDT variable: ICE in
! gfc_spec_list_type (expr.cc), called from the hidden-PDT-argument code
! in gfc_conv_procedure_call, which takes the base variable's param_list
! ('m' of c_t) and looks it up in the component's type (h_t)
!
! Regression from r17 6335154826f, PDT (9/16) [PR82173]; works with 16.2.1.
! Found building fiats (src/fiats/training_configuration_s.F90:36).
!
! Contributed by Jerry Delisle  <jvdelisle@gcc.gnu.org>
!
module pdtmod
  implicit none
  type h_t(k)
    integer, kind :: k = kind(1.0)
    real(k) :: x = real(1, k)
  contains
    generic :: to_i => r4_to_i, r8_to_i
    procedure, private :: r4_to_i, r8_to_i
  end type
  type c_t(m)
    integer, kind :: m = kind(1.0)
    type(h_t(m)) :: h
  end type
contains
  pure integer function r4_to_i (self)
    class(h_t(kind(1.0))), intent(in) :: self
    r4_to_i = int (self%x)
  end function
  pure integer function r8_to_i (self)
    class(h_t(kind(1.0d0))), intent(in) :: self
    r8_to_i = 2 * int (self%x)
  end function
end module pdtmod
program p
  use pdtmod
  implicit none
  type(c_t) :: c
  type(c_t(kind(1.0d0))) :: d
  if (c%h%to_i () /= 1) stop 1
  if (d%h%to_i () /= 2) stop 2
end program
