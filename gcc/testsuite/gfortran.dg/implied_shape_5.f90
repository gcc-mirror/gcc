! { dg-do compile }
!
! PR fortran/68020
!
! Reject mixing explicit-shape and implied-shape arrays
!
subroutine rank_1_2
  integer, parameter :: a(1, 2) = 0
  integer, parameter :: y(11:11, 12:*) = a ! { dg-error "Assumed size array at .1. must be a dummy argument" }
  integer, parameter :: x(:, *) = a ! { dg-error "Bad specification for deferred shape array" }
end

subroutine rank_3
  integer, parameter :: a(1, 2, 3) = 0
  integer, parameter :: y(11:*, 12:14, 13:*) = a  ! { dg-error "Bad specification for assumed size array" }
  integer, parameter :: x(11:*, :, 13:*) = a      ! { dg-error "Bad specification for assumed size array" }
end

subroutine rank_4
  integer, parameter :: a(1, 2, 3, 4) = 0
  integer, parameter :: y(11:*, 12:*, 13:*, 14:17) = a ! { dg-error "Bad array specification for implied-shape array" }
  integer, parameter :: y(11:*, 12:*, 13:*, 14:) = a   ! { dg-error "Bad array specification for implied-shape array" }
end

program p
  call rank_1_2
  call rank_3
  call rank_4
end program p
