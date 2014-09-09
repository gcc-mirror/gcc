! { dg-do compile }
! { dg-options "-fcray-pointer" }
!
! PR fortran/62174
! Component declarations within derived types would overwrite the typespec of
! variables with the same name who were Cray pointees.
implicit none

type t1
  integer i
end type t1
type(t1) x

pointer (x_ptr, x)

type t2
  real x ! should not overwrite x's type
end type t2

x%i = 0 ! should see no error here

end
