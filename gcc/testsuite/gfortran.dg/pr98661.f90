! { dg-do compile }
! PR fortran/98661 - valgrind issues with error recovery
!
! Test issues related to former testcase charlen_03.f90
program p
  implicit none
  type t
     character(:), pointer :: c(n) ! { dg-error "must have a deferred shape" }
     real,     allocatable :: x(n) ! { dg-error "must have a deferred shape" }
  end type
end

subroutine s
! no 'implicit none'
  type u
     character(:), pointer :: c(n) ! { dg-error "must have a deferred shape" }
     real,     allocatable :: x(n) ! { dg-error "must have a deferred shape" }
  end type
end
