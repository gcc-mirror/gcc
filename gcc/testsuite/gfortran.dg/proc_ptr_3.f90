! { dg-do run }
!
! PROCEDURE POINTERS without the PROCEDURE statement
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

real function e1(x)
  real :: x
  print *,'e1!',x
  e1 = x * 3.0
end function

subroutine e2(a,b)
  real, intent(inout) :: a
  real, intent(in) :: b
  print *,'e2!',a,b
  a = a + b
end subroutine

program proc_ptr_3

real, external, pointer :: fp

pointer :: sp
interface
  subroutine sp(a,b)
    real, intent(inout) :: a
    real, intent(in) :: b
  end subroutine sp
end interface

external :: e1,e2
real :: c = 1.2

fp => e1

if (abs(fp(2.5)-7.5)>0.01) call abort()

sp => e2

call sp(c,3.4)

if (abs(c-4.6)>0.01) call abort()

end
