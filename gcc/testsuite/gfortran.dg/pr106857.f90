! { dg-do compile }
! PR fortran/106857 - ICE in gfc_simplify_pack
! Contributed by G.Steinmetz

program p
  type t
     integer :: n
  end type
  type(t), parameter :: a(2,2) = t(1)
  type(t), parameter :: b(4) = reshape(a, [2])                          ! { dg-error "Different shape" }
  type(t), parameter :: c(2) = pack(b, [.false.,.true.,.false.,.true.]) ! { dg-error "Different shape" }
end
