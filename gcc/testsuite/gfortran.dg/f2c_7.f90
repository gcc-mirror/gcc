! { dg-do run }
! { dg-options "-ff2c" }
! Verifies that array results work with -ff2c
! try all permutations of result clause in function yes/no
!                     and result clause in interface yes/no
! this is not possible in Fortran 77, but this exercises a previously
! buggy codepath
function c() result (r)
  complex :: r(5)
  r = 0.
end function c

function d()
  complex :: d(5)
  d = 1.
end function d

subroutine test_without_result
interface
   function c
     complex :: c(5)
   end function c
end interface
interface
   function d
     complex :: d(5)
   end function d
end interface
complex z(5)
z = c()
if (any(z /= 0.)) call abort ()
z = d()
if (any(z /= 1.)) call abort ()
end subroutine test_without_result

subroutine test_with_result
interface
   function c result(r)
     complex :: r(5)
   end function c
end interface
interface
   function d result(r)
     complex :: r(5)
   end function d
end interface
complex z(5)
z = c()
if (any(z /= 0.)) call abort ()
z = d()
if (any(z /= 1.)) call abort ()
end subroutine test_with_result

call test_without_result
call test_with_result
end
  
