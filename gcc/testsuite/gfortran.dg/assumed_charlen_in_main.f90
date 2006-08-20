! { dg-do compile }
! Tests the fix for PR28771 in which an assumed character length variable with an initializer could
! survive in the main program without causing an error.
!
! Contributed by Martin Reinecke  <martin@mpa-garching.mpg.de>
!
program test
  character(len=*), parameter :: foo = 'test'     ! Parameters must work.
  character(len=4) :: bar = foo
  character(len=*) :: foobar = 'This should fail' ! {  dg-error "must be a dummy" }
  print *, bar
end

