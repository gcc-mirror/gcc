! { dg-do compile }
program a
  real x, y(2)
  x = 1./3.
  y = [1, 2] / 3.
  print *, nearest(x, 0.)              ! { dg-error "shall not be zero" }
  print *, nearest(y, 0.)              ! { dg-error "shall not be zero" }
  print *, nearest([1., 2.] / 3., 0.)  ! { dg-error "shall not be zero" }
  print *, nearest(1., 0.)             ! { dg-error "shall not be zero" }
end program a
