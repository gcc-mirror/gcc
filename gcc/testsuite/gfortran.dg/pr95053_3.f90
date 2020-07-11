! { dg-do compile }
! Related to PR 93499 - this used to ICE.

program p
  type t(n)
     integer, kind :: n
  end type t
  type u(n)
     integer, len :: n
  end type u
  type(t((0)/0))  :: x  ! { dg-error "does not simplify to an INTEGER" }
  type(t((0.)/0)) :: y  ! { dg-error "must be of INTEGER type" }
  type(u(0/(0.))) :: z  ! { dg-error "must be of INTEGER type" }
end
