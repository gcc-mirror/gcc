! { dg-do compile }
! PR fortran/95978 - ICE in gfc_match_data, at fortran/decl.c:731

program p
  type t
     integer :: a
     type(t), allocatable :: b
     data c /t(1)/               ! { dg-error "Unexpected DATA statement" }
  end type t
end
