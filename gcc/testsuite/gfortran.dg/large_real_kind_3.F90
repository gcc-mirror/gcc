! { dg-do run }
! { dg-require-effective-target fortran_large_real }

! Testing erf and erfc library calls on large real kinds (larger than kind=8)
  implicit none

  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  real(8),parameter :: eps = 1e-8

  real(kind=k) :: x
  real(8) :: y

#define TEST_FUNCTION(func,val) \
 x = val ;\
 y = x ;\
 x = func (x) ;\
 y = func (y) ;\
 if (abs((y - x) / y) > eps) call abort
  
 TEST_FUNCTION(erf,1.45123231)
 TEST_FUNCTION(erfc,-0.123789)
 
end
