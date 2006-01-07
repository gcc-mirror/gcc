!==================assumed_size_refs_1.f90==================
! { dg-do compile }
! Test the fix for PR25029, PR21256 in which references to
! assumed size arrays without an upper bound to the last
! dimension were generating no error. The first version of
! the patch failed in DHSEQR, as pointed out by Toon Moene
! in http://gcc.gnu.org/ml/fortran/2005-12/msg00466.html
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
program assumed_size_test_1
  implicit none
  real a(2, 4)

  a = 1.0
  call foo (a)

contains
  subroutine foo(m)
    real, target :: m(1:2, *)
    real x(2,2,2)
    real, external :: bar
    real, pointer :: p(:,:), q(:,:)
    allocate (q(2,2))

! PR25029
    p => m                     ! { dg-error "upper bound in the last dimension" }
    q = m                      ! { dg-error "upper bound in the last dimension" }

! PR21256( and PR25060)
    m = 1                      ! { dg-error "upper bound in the last dimension" }

    m(1,1) = 2.0
    x = bar (m)
    x = fcn (m)                ! { dg-error "upper bound in the last dimension" }
    m(:, 1:2) = fcn (q)
    call sub (m, x)            ! { dg-error "upper bound in the last dimension" }
    call sub (m(1:2, 1:2), x)
    print *, p

    call DHSEQR(x)

  end subroutine foo

  elemental function fcn (a) result (b)
    real, intent(in) :: a
    real :: b
    b = 2.0 * a
  end function fcn

  elemental subroutine sub (a, b)
    real, intent(inout) :: a, b
    b = 2.0 * a
  end subroutine sub
  
  SUBROUTINE DHSEQR( WORK )
    REAL WORK( * )
    EXTERNAL           DLARFX
    INTRINSIC          MIN
    WORK( 1 ) = 1.0
    CALL DLARFX( MIN( 1, 8 ), WORK )
  END SUBROUTINE DHSEQR

end program assumed_size_test_1
