! { dg-do compile }
!
! PR fortran/35033
!
! The checks for assignments were too strict.
!
MODULE m1
          INTERFACE ASSIGNMENT(=)
             SUBROUTINE s(a,b)
                 REAL,INTENT(OUT) :: a(1,*)
                 REAL,INTENT(IN) :: b(:)
             END SUBROUTINE
          END Interface
contains
  subroutine test1()
          REAL,POINTER :: p(:,:),q(:)
          CALL s(p,q) 
          p = q
  end subroutine test1
end module m1

MODULE m2
          INTERFACE ASSIGNMENT(=)
             SUBROUTINE s(a,b)
                 REAL,INTENT(OUT),VOLATILE :: a(1,*)
                 REAL,INTENT(IN) :: b(:)
             END SUBROUTINE
          END Interface
contains
  subroutine test1()
          REAL,POINTER :: p(:,:),q(:)
          CALL s(p,q) ! { dg-error "requires an assumed-shape or pointer-array dummy" }
!TODO: The following is rightly rejected but the error message is misleading.
! The actual reason is the mismatch between pointer array and VOLATILE
          p = q ! { dg-error "Incompatible ranks" }
  end subroutine test1
end module m2

MODULE m3
          INTERFACE ASSIGNMENT(=)
             module procedure s
          END Interface
contains
             SUBROUTINE s(a,b) ! { dg-error "must not redefine an INTRINSIC type" }
                 REAL,INTENT(OUT),VOLATILE :: a(1,*)
                 REAL,INTENT(IN) :: b(:,:)
             END SUBROUTINE
end module m3

