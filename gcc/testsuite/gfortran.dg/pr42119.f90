! { dg-do compile }

module Test
use ISO_C_BINDING

contains

subroutine Callback(arg) bind(C)
  integer(C_INT)  :: arg
end subroutine Callback

subroutine Check(proc)
  type(C_FUNPTR)  :: proc
end subroutine Check

end module Test


program Main
  use Test
  type(C_FUNPTR)  :: proc

  call Check(C_FUNLOC(Callback))
end program Main
! { dg-final { cleanup-modules "test" } }
