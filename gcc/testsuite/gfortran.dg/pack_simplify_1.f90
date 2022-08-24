! { dg-do compile }
! PR fortran/106049 - ICE in gfc_simplify_pack
! Contributed by G.Steinmetz

program p
  type t
  end type
  logical, parameter :: m(0) = [ logical :: ]
  type(t), parameter :: a(0) = [ t :: ]
  type(t), parameter :: b(1) = [ t()  ]
  type(t), parameter :: c(1) = [ t :: ]        ! { dg-error "Different shape" }
  type(t), parameter :: d(0) = pack(a, m)
  type(t), parameter :: e(1) = pack(b, [.true.])
  type(t), parameter :: f(1) = pack(c, [.true.])
end
