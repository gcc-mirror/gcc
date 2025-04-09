! { dg-do compile }
! { dg-options "-std=f2018" }
program do_concurrent_default_none
  implicit none
  integer :: i, x, y, z
  x = 0
  y = 0
  z = 0
  do concurrent (i = 1:10) default(none) shared(x) local(y)
    ! { dg-error "Variable 'z' .* not specified in a locality spec .* but required due to DEFAULT \\(NONE\\)" "" { target *-*-* } .-1 }
    x = x + i
    y = i * 2
    z = z + 1 ! { dg-error "Variable 'z' .* not specified in a locality spec .* but required due to DEFAULT \\(NONE\\)" }
  end do
end program do_concurrent_default_none
