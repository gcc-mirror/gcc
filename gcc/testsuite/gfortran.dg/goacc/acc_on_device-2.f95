! Have to enable optimizations, as otherwise builtins won't be expanded.
! { dg-additional-options "-O -fdump-rtl-expand" }

module openacc_kinds
  implicit none

  integer, parameter :: acc_device_kind = 4

end module openacc_kinds

module openacc
  use openacc_kinds
  implicit none

  integer (acc_device_kind), parameter :: acc_device_host = 2

  interface
     function acc_on_device (dev)
       use openacc_kinds
       logical (4) :: acc_on_device
       integer (acc_device_kind), intent (in) :: dev
     end function acc_on_device
  end interface
end module openacc

logical (4) function f ()
  use openacc
  implicit none

  integer (4), parameter :: dev = 2

  f = acc_on_device (dev)
  return
end function f

! With -fopenacc, we're expecting the builtin to be expanded, so no calls.
! TODO: not working.
! { dg-final { scan-rtl-dump-times "\\\(call \[^\\n\]* acc_on_device" 0 "expand" { xfail *-*-* } } }

