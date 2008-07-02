! { dg-do run }
!
! PROCEDURE POINTERS as actual/formal arguments
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

subroutine foo(j)
  INTEGER, INTENT(OUT) :: j
  j = 6
end subroutine

program proc_ptr_6

PROCEDURE(),POINTER :: ptr1
PROCEDURE(REAL),POINTER :: ptr2
EXTERNAL foo
INTEGER :: k = 0

ptr1 => foo
call s_in(ptr1,k)
if (k /= 6) call abort()

call s_out(ptr2)
if (ptr2(-3.0) /= 3.0) call abort()

contains

subroutine s_in(p,i)
  PROCEDURE(),POINTER,INTENT(IN) :: p
  INTEGER, INTENT(OUT) :: i
  call p(i)
end subroutine

subroutine s_out(p)
  PROCEDURE(REAL),POINTER,INTENT(OUT) :: p
  p => abs
end subroutine

end program
