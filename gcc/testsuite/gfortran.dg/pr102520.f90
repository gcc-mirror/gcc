! { dg-do compile }
! PR fortran/102520 - ICE in expand_constructor, at fortran/array.c:1802

program p
  type t
  end type
  type(t), parameter :: a(4)   = shape(1)         ! { dg-error "Incompatible" }
  type(t), parameter :: b(2,2) = reshape(a,[2,2]) ! { dg-error "must be an array" }
  type(t), parameter :: c(2,2) = transpose(b)     ! { dg-error "must be of rank 2" }
end
