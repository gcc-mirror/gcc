! PR 30319 - Bogus warning from -Warray-bounds during string assignment
! { dg-do compile }
! { dg-options "-O2 -Warray-bounds" }

program test_bounds

  character(256) :: foo

  foo = '1234'                 ! { dg-bogus "\\\[-Warray-bounds" }

  print *, foo

end program test_bounds
