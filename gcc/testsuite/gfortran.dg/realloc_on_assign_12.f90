! { dg-do run }
!
! PR fortran/52151
!
! Check that the bounds/shape/strides are correctly set
! for (re)alloc on assignment, if the LHS is either not
! allocated or has the wrong shape. This test is for
! code which is only invoked for libgfortran intrinsic
! such as RESHAPE.
!
! Based on the example of PR 52117 by Steven Hirshman
!
    PROGRAM RESHAPEIT
      call unalloc ()
      call wrong_shape ()
    contains
    subroutine unalloc ()
      INTEGER, PARAMETER :: n1=2, n2=2, n3=2
      INTEGER            :: m1, m2, m3, lc
      REAL, ALLOCATABLE  :: A(:,:), B(:,:,:)
      REAL               :: val

      ALLOCATE (A(n1,n2*n3))
! << B is not allocated

      val = 0
      lc = 0
      DO m3=1,n3
         DO m2=1,n2
            lc = lc+1
            DO m1=1,n1
               val = val+1
               A(m1, lc) = val
            END DO
         END DO
      END DO

      B = RESHAPE(A, [n1,n2,n3])

      if (any (shape (B)  /= [n1,n2,n3])) STOP 1
      if (any (ubound (B) /= [n1,n2,n3])) STOP 2
      if (any (lbound (B) /= [1,1,1])) STOP 3

      lc = 0
      DO m3=1,n3
         DO m2=1,n2
            lc = lc+1
            DO m1=1,n1
!               PRINT *,'A(',m1,',',lc,') = ',A(m1,lc),' B = ',B(m1,m2,m3)
               if (A(m1,lc) /= B(m1,m2,m3)) STOP 4
            END DO
         END DO
      END DO
      DEALLOCATE(A, B)
    end subroutine unalloc

    subroutine wrong_shape ()
      INTEGER, PARAMETER :: n1=2, n2=2, n3=2
      INTEGER            :: m1, m2, m3, lc
      REAL, ALLOCATABLE  :: A(:,:), B(:,:,:)
      REAL               :: val

      ALLOCATE (A(n1,n2*n3))
      ALLOCATE (B(1,1,1))     ! << shape differs from RHS

      val = 0
      lc = 0
      DO m3=1,n3
         DO m2=1,n2
            lc = lc+1
            DO m1=1,n1
               val = val+1
               A(m1, lc) = val
            END DO
         END DO
      END DO

      B = RESHAPE(A, [n1,n2,n3])

      if (any (shape (B)  /= [n1,n2,n3])) STOP 5
      if (any (ubound (B) /= [n1,n2,n3])) STOP 6
      if (any (lbound (B) /= [1,1,1])) STOP 7

      lc = 0
      DO m3=1,n3
         DO m2=1,n2
            lc = lc+1
            DO m1=1,n1
!               PRINT *,'A(',m1,',',lc,') = ',A(m1,lc),' B = ',B(m1,m2,m3)
               if (A(m1,lc) /= B(m1,m2,m3)) STOP 8
            END DO
         END DO
      END DO
      DEALLOCATE(A, B)
    end subroutine wrong_shape
    END PROGRAM RESHAPEIT
