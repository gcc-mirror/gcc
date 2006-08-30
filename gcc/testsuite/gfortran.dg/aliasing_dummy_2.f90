! { dg-do compile }
! This tests the fix for PR28885, in which multiple calls to a procedure
! with different components of an array of derived types for an INTENT(OUT)
! argument caused an ICE internal compiler error.  This came about because
! the compiler would lose the temporary declaration with each subsequent
! call of the procedure.
!
! Reduced from the contribution by Drew McCormack  <drewmccormack@mac.com>
!
program test
  type t
    integer :: i
    integer :: j
  end type
  type (t) :: a(5) 
  call sub('one',a%j)
  call sub('two',a%i)
contains
  subroutine sub(key,a)
    integer, intent(out)    :: a(:) 
    character(*),intent(in) :: key
    a = 1   
  end subroutine
end program 
