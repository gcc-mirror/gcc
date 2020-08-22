! { dg-do compile }
! PR fortran/95587 - ICE in gfc_target_encode_expr, at fortran/target-memory.c:362

program p
  type t
  end type t
  class(*), allocatable :: x, y
  class(t), allocatable :: u, v
  class(t),    pointer  :: c, d
  equivalence (x, y) ! { dg-error "conflicts with ALLOCATABLE" }
  equivalence (u, v) ! { dg-error "conflicts with ALLOCATABLE" }
  equivalence (c, d) ! { dg-error "conflicts with POINTER" }
end
