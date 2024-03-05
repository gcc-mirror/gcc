! { dg-do compile }
! { dg-additional-options "-frange-check" }
!
! PR fortran/103707
! PR fortran/106987
!
! Check error recovery on arithmetic exceptions

program p
  implicit none
  integer, parameter :: a(3) = [30,31,32]
  integer, parameter :: e(1) = 2
  print *, 2 ** a       ! { dg-error "Arithmetic overflow" }
  print *, e ** 31      ! { dg-error "Arithmetic overflow" }
end

! { dg-prune-output "Result of exponentiation" }

subroutine s
  implicit none
  real, parameter :: inf = real (z'7F800000')
  real, parameter :: nan = real (z'7FC00000')

  ! Unary operators
  print *, -[inf,nan]           ! { dg-error "Arithmetic overflow" }
  print *, -[nan,inf]           ! { dg-error "Arithmetic NaN" }

  ! Binary operators
  print *, [1.]/[0.]            ! { dg-error "Division by zero" }
  print *, [0.]/[0.]            ! { dg-error "Arithmetic NaN" }
  print *, 0. / [(0.,0.)]       ! { dg-error "Arithmetic NaN" }
  print *, [1.,0.]/[0.,0.]      ! { dg-error "Division by zero" }
  print *, [(1.,1.)]/[0.]       ! { dg-error "Division by zero" }
  print *, [(1.,0.)]/[0.]       ! { dg-error "Division by zero" }
  print *, [(0.,0.)]/[0.]       ! { dg-error "Arithmetic NaN" }
  print *, - [1./0.]/[0.]       ! { dg-error "Division by zero" }
  print *, - [ 1/0 ] * 1        ! { dg-error "Division by zero" }

  ! Binary operators, exceptional input
  print *, 1. / nan             ! { dg-error "Arithmetic NaN" }
  print *, [inf] / inf          ! { dg-error "Arithmetic NaN" }
  print *, inf + [nan]          ! { dg-error "Arithmetic NaN" }
  print *, [(1.,0.)]/[(nan,0.)] ! { dg-error "Arithmetic NaN" }
  print *, [(1.,0.)]/[(0.,nan)] ! { dg-error "Arithmetic NaN" }
  print *, [(1.,0.)]/[(inf,0.)] ! OK
  print *, [nan,inf] / (0.)     ! { dg-error "Arithmetic NaN" }
  print *, [inf,nan] / (0.)     ! { dg-error "Arithmetic overflow" }
end
