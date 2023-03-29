! { dg-do compile }
! PR fortran/107054 - ICE in gfc_simplify_unpack
! Contributed by G.Steinmetz

program p
  type t
     integer :: n = 0
  end type
  type(t), parameter :: a(4) = t(2)
  type(t), parameter :: b(4) = reshape(a,[2]) ! { dg-error "Different shape" }
  type(t), parameter :: c(2) = pack(b,[.false.,.true.,.false.,.true.]) ! { dg-error "Different shape" }
  type(t), parameter :: d(4) = unpack(c,[.false.,.true.,.false.,.true.],a)
end
