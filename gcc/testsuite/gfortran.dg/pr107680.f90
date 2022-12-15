! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/107680 - ICE in arith_power
! Contributed by G.Steinmetz

program p
  real,    parameter :: x(*) = [real    :: ([1])]   **  2.0
  complex, parameter :: y(*) = [real    :: ([1])]   ** (2.0,1.0)
  complex, parameter :: z(*) = [complex :: ([1])]   ** (2.0,1.0)
  complex, parameter :: u(*) = [complex :: ([1.0])] ** (2.0,1.0)
  complex, parameter :: v(*) = [real    :: ([(1.0,2.0)])] ** (3.0,1.0)
  complex, parameter :: w(*) = [integer :: ([(1.0,2.0)])] ** (3.0,1.0)
  print *, [real    :: ([3])]   **  2
  print *, [real    :: ([3])]   **  2.0
  print *, [real    :: ([1])]   ** (1.0,2.0)
  print *, [real    :: ([1.0])] ** (1.0,2.0)
  print *, [complex :: ([3])]   **  2
  print *, [complex :: ([3])]   **  2.0
  print *, [complex :: ([1])]   ** (1.0,2.0)
  print *, [complex :: ([1.0])] ** (1.0,2.0)
  print *, [integer :: ([3.0])] **  2
  print *, [integer :: ([3.0])] **  2.0
  print *, [integer :: ([1.0])] ** (1.0,2.0)
  print *, [integer :: ([(1.0,2.0)])] ** (3.0,1.0)
  print *, v(1)
  if (u(1) /= 1) stop 1
  if (v(1) /= 1) stop 2
  if (w(1) /= 1) stop 3
  if (x(1) /= 1) stop 4
  if (y(1) /= 1) stop 5
  if (z(1) /= 1) stop 6
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
