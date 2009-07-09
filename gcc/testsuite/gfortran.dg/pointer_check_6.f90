! { dg-do run }
! { dg-options "-fcheck=pointer" }
!
! { dg-shouldfail "pointer check" }
! { dg-output ".*At line 104 of file .*Fortran runtime error: Pointer actual argument 'a' is not associated.*" }
!
! PR fortran/40604
!
! The following cases are all valid, but were failing
! for one or the other reason.
!
! Contributed by Janus Weil and Tobias Burnus.
!

subroutine test1()
  call test(uec=-1)
contains 
  subroutine test(str,uec)
    implicit none
    character*(*), intent(in), optional:: str
    integer, intent(in), optional :: uec
  end subroutine
end subroutine test1

module m
  interface matrixMult
     Module procedure matrixMult_C2
  End Interface
contains
  subroutine test
    implicit none
    complex, dimension(0:3,0:3) :: m1,m2
    print *,Trace(MatrixMult(m1,m2))
  end subroutine
  complex function trace(a)
    implicit none
    complex, intent(in),  dimension(0:3,0:3) :: a 
  end function trace
  function matrixMult_C2(a,b) result(matrix)
    implicit none
    complex, dimension(0:3,0:3) :: matrix,a,b
  end function matrixMult_C2
end module m

SUBROUTINE plotdop(amat)
      IMPLICIT NONE
      REAL,    INTENT (IN) :: amat(3,3)
      integer :: i1
      real :: pt(3)
      i1 = 1
      pt = MATMUL(amat,(/i1,i1,i1/))
END SUBROUTINE plotdop

        FUNCTION evaluateFirst(s,n)result(number)
          IMPLICIT NONE
          CHARACTER(len =*), INTENT(inout) :: s
          INTEGER,OPTIONAL                 :: n
          REAL                             :: number
          number = 1.1
        end function

SUBROUTINE rw_inp(scpos)
      IMPLICIT NONE
      REAL scpos

      interface
        FUNCTION evaluateFirst(s,n)result(number)
          IMPLICIT NONE
          CHARACTER(len =*), INTENT(inout) :: s
          INTEGER,OPTIONAL                 :: n
          REAL                             :: number
        end function
      end interface

      CHARACTER(len=100) :: line
      scpos = evaluatefirst(line)
END SUBROUTINE rw_inp

program test
  integer, pointer :: a
!  nullify(a)
  allocate(a)
  a = 1
  call sub1a(a)
  call sub1b(a)
  call sub1c()
contains
  subroutine sub1a(a)
   integer, pointer :: a
   call sub2(a)
   call sub3(a)
   call sub4(a)
  end subroutine sub1a
  subroutine sub1b(a)
   integer, pointer,optional :: a
   call sub2(a)
   call sub3(a)
   call sub4(a)
  end subroutine sub1b
  subroutine sub1c(a)
   integer, pointer,optional :: a
   call sub4(a)
!   call sub2(a)  ! << Invalid - working correctly, but not allowed in F2003
   call sub3(a) ! << INVALID
  end subroutine sub1c
  subroutine sub4(b)
    integer, optional,pointer :: b
  end subroutine
  subroutine sub2(b)
    integer, optional :: b
  end subroutine
  subroutine sub3(b)
    integer :: b
  end subroutine
end


! { dg-final { cleanup-modules "m" } }
