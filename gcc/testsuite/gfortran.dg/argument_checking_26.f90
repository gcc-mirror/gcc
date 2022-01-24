! { dg-do compile }
! PR fortran/104212 - ICE in transformational_result
! Contributed by G.Steinmetz

program p
  logical, parameter :: a(*,*) = reshape([.true.,.false.], shape=[1,2])
  real,    parameter :: r(*,*) = reshape([1.,2.], shape=[1,2])
  print *, parity(a)
  print *, parity(a, dim=1)
  print *, parity(a, dim=[1]) ! { dg-error "must be a scalar" }
  print *, norm2 (r)
  print *, norm2 (r, dim=1)
  print *, norm2 (r, dim=[1]) ! { dg-error "must be a scalar" }
end
