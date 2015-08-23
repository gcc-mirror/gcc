! { dg-do run }
! 
! Check that BACKTRACE is available on all targets. We cannot actually
! check its output, but we should at least be able to call it, then exit
! normally.
!
program test
  call backtrace
  stop
end program test
