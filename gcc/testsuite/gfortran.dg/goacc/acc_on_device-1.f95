! Have to enable optimizations, as otherwise builtins won't be expanded.
! { dg-additional-options "-O -fdump-rtl-expand" }

logical function f ()
  implicit none

  external acc_on_device
  logical (4) acc_on_device

  f = .false.
  f = f .or. acc_on_device ()
  f = f .or. acc_on_device (1, 2)
  f = f .or. acc_on_device (3.14)
  f = f .or. acc_on_device ("hello")

  return
end function f

! Unsuitable to be handled as a builtin, so we're expecting four calls.
! { dg-final { scan-rtl-dump-times "\\\(call \[^\\n\]* acc_on_device" 4 "expand" } }

