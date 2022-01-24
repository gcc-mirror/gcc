! { dg-additional-options -Wuninitialized }

type t2
  integer :: bar
end type t2
type t
  type(t2), pointer :: foo
end type t

type(t) :: c
! { dg-note {'c' declared here} {} { target *-*-* } .-1 }

!$acc enter data copyin(c%foo)
! { dg-warning {'c\.foo' is used uninitialized} {} { target *-*-* } .-1 }

end
