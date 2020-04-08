! { dg-do compile }
! PR fortran/93686

type t
end type
type(t), pointer :: x
data x / ! { dg-error "part-ref with pointer attribute near ... is not rightmost part-ref of data-stmt-object" }
end
