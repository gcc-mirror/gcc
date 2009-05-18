! { dg-do run }
!
! PR 40164: Fortran 2003: "Arrays of procedure pointers" (using PPCs)
!
! Original test case by Barron Bichon <barron.bichon@swri.org>
! Adapted by Janus Weil <janus@gcc.gnu.org>

PROGRAM test_prog

  ABSTRACT INTERFACE
  FUNCTION fn_template(n,x) RESULT(y)
    INTEGER, INTENT(in) :: n
    REAL, INTENT(in) :: x(n)
    REAL :: y(n)
  END FUNCTION fn_template
  END INTERFACE

  TYPE PPA
    PROCEDURE(fn_template), POINTER, NOPASS :: f
  END TYPE PPA

 TYPE ProcPointerArray
   PROCEDURE(add), POINTER, NOPASS :: f
 END TYPE ProcPointerArray

 TYPE (ProcPointerArray) :: f_array(3)
 PROCEDURE(add), POINTER :: f
 real :: r

 f_array(1)%f => add
 f => f_array(1)%f
 f_array(2)%f => sub
 f_array(3)%f => f_array(1)%f

 r = f(1.,2.)
 if (abs(r-3.)>1E-3) call abort()
 r = f_array(1)%f(4.,2.)
 if (abs(r-6.)>1E-3) call abort()
 r = f_array(2)%f(5.,3.)
 if (abs(r-2.)>1E-3) call abort()
 if (abs(f_array(1)%f(1.,3.)-f_array(3)%f(2.,2.))>1E-3) call abort()

CONTAINS

 FUNCTION add(a,b) RESULT(sum)
   REAL, INTENT(in) :: a, b
   REAL :: sum
   sum = a + b
 END FUNCTION add

 FUNCTION sub(a,b) RESULT(diff)
   REAL, INTENT(in) :: a, b
   REAL :: diff
   diff = a - b
 END FUNCTION sub

END PROGRAM test_prog

