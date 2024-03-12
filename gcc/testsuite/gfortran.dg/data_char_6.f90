! { dg-do compile }
! PR fortran/68569 - ICE with automatic character object and DATA 
! Contributed by G. Steinmetz

subroutine s1 (n)
  implicit none
  integer, intent(in) :: n
  character(n) :: x
  data x /'a'/         ! { dg-error "Non-constant character length" }
end

subroutine s2 (n)
  implicit none
  integer, intent(in) :: n
  character(n) :: x
  data x(1:1) /'a'/    ! { dg-error "Non-constant character length" }
end

subroutine s3 ()
  implicit none
  type t
     character(:) :: c ! { dg-error "must be a POINTER or ALLOCATABLE" }
  end type t
  type(t) :: tp
  data tp%c /'a'/      ! { dg-error "Non-constant character length" }
end
