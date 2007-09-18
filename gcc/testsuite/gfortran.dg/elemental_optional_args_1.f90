! { dg-do compile }
! { dg-options "-pedantic" }
! Check the fix for PR20893, in which actual arguments could violate: 
! "(5) If it is an array, it shall not be supplied as an actual argument to
! an elemental procedure unless an array of the same rank is supplied as an
! actual argument corresponding to a nonoptional dummy argument of that 
! elemental procedure." (12.4.1.5)
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  CALL T1(1,2)
CONTAINS
  SUBROUTINE T1(A1,A2,A3)
    INTEGER           :: A1,A2, A4(2), A5(2)
    INTEGER, OPTIONAL :: A3(2)
    interface
      elemental function efoo (B1,B2,B3) result(bar)
        INTEGER, intent(in)           :: B1, B2
        integer           :: bar
        INTEGER, OPTIONAL, intent(in) :: B3
      end function efoo
    end interface

! check an intrinsic function
    write(6,*) MAX(A1,A2,A3) ! { dg-warning "array and OPTIONAL" }
    write(6,*) MAX(A1,A3,A2)
    write(6,*) MAX(A1,A4,A3)
! check an internal elemental function
    write(6,*) foo(A1,A2,A3) ! { dg-warning "array and OPTIONAL" }
    write(6,*) foo(A1,A3,A2)
    write(6,*) foo(A1,A4,A3)
! check an external elemental function
    write(6,*) efoo(A1,A2,A3) ! { dg-warning "array and OPTIONAL" }
    write(6,*) efoo(A1,A3,A2)
    write(6,*) efoo(A1,A4,A3)
! check an elemental subroutine
    call foobar (A5,A2,A4)
    call foobar (A5,A4,A4)
  END SUBROUTINE
  elemental function foo (B1,B2,B3) result(bar)
    INTEGER, intent(in)           :: B1, B2
    integer           :: bar
    INTEGER, OPTIONAL, intent(in) :: B3
    bar = 1
  end function foo
  elemental subroutine foobar (B1,B2,B3)
    INTEGER, intent(OUT)           :: B1
    INTEGER, optional, intent(in)  :: B2, B3
    B1 = 1
  end subroutine foobar

END

