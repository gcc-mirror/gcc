! PR 101334
! PR 101337
! { dg-do compile}
! { dg-additional-options "-fcoarray=single" }
!
! TS 29113
! C535b An assumed-rank variable name shall not appear in a designator
! or expression except as an actual argument corresponding to a dummy 
! argument that is assumed-rank, the argument of the C_LOC function
! in the ISO_C_BINDING intrinsic module, or the first argument in a
! reference to an intrinsic inquiry function.
!
! This has been renamed C838 in the Fortran 2018 standard, with C_SIZEOF
! and SELECT_RANK additionally added.
!
! This test file contains tests that are expected to issue diagnostics
! for invalid code.

! Check that passing an assumed-rank variable as an actual argument 
! corresponding to a non-assumed-rank dummy gives a diagnostic.

module m
  interface
    subroutine f (a, b)
      implicit none
      integer :: a
      integer :: b
    end subroutine
    subroutine g (a, b)
      implicit none
      integer :: a(..)
      integer :: b(..)
    end subroutine
    subroutine h (a, b)
      implicit none
      integer :: a(*)
      integer :: b(*)
    end subroutine
    subroutine i (a, b)
      implicit none
      integer :: a(:)
      integer :: b(:)
    end subroutine
    subroutine j (a, b)
      implicit none
      integer :: a(3,3)
      integer :: b(3,3)
    end subroutine
  end interface
end module

subroutine test_calls (x, y)
  use m
  implicit none
  integer :: x(..), y(..)

  ! Make sure each invalid argument produces a diagnostic.
  ! scalar dummies
  call f (x, &  ! { dg-error "(A|a)ssumed.rank" }
          y)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  ! assumed-rank dummies
  call g (x, y)  ! OK
  ! assumed-size dummies
  call h (x, &  ! { dg-error "(A|a)ssumed.rank" "pr101334" }
          y)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  ! assumed-shape dummies
  call i (x, &  ! { dg-error "(A|a)ssumed.rank" }
          y)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
 ! fixed-size array dummies
  call j (x, &  ! { dg-error "(A|a)ssumed.rank" "pr101334" }
          y)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
end subroutine

! Check that you can't use an assumed-rank array variable in an array
! element or section designator.

subroutine test_designators (x)
  use m
  implicit none
  integer :: x(..)

  call f (x(1), 1)  ! { dg-error "(A|a)ssumed.rank" }
  call g (x(1:3:1), &  ! { dg-error "(A|a)ssumed.rank" }
          x)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
end subroutine

! Check that you can't use an assumed-rank array variable in elemental
! expressions.  Make sure binary operators produce the error for either or
! both operands.

subroutine test_expressions (a, b, c, l, m, n, x, y, z, p, q, r, s, i, j)
  implicit none
  integer :: a(..), b(..), c(..)
  logical :: l(..), m(..), n(..)
  integer :: x(s), y(s), z(s)
  logical :: p(s), q(s), r(s)
  integer :: s
  integer :: i
  logical :: j

  ! Assignment

  z = x  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a  ! { dg-error "(A|a)ssumed.rank" }
  z = i  ! OK
  c = i  ! { dg-error "(A|a)ssumed.rank" }

  r = p  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l  ! { dg-error "(A|a)ssumed.rank" }
  r = j  ! OK
  n = j  ! { dg-error "(A|a)ssumed.rank" }

  ! Arithmetic

  z = -x  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = -a  ! { dg-error "(A|a)ssumed.rank" }
  z = -i  ! OK
  c = -i  ! { dg-error "(A|a)ssumed.rank" }

  z = x + y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    + b  ! { dg-error "(A|a)ssumed.rank" }
  z = x + i  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a + i  ! { dg-error "(A|a)ssumed.rank" }
  z = i + y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = i + b  ! { dg-error "(A|a)ssumed.rank" }

  z = x - y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    - b  ! { dg-error "(A|a)ssumed.rank" }
  z = x - i  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a - i  ! { dg-error "(A|a)ssumed.rank" }
  z = i - y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = i - b  ! { dg-error "(A|a)ssumed.rank" }

  z = x * y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    * b  ! { dg-error "(A|a)ssumed.rank" }
  z = x * i  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a * i  ! { dg-error "(A|a)ssumed.rank" }
  z = i * y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = i * b  ! { dg-error "(A|a)ssumed.rank" }

  z = x / y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    / b  ! { dg-error "(A|a)ssumed.rank" }
  z = x / i  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a / i  ! { dg-error "(A|a)ssumed.rank" }
  z = i / y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = i / b  ! { dg-error "(A|a)ssumed.rank" }

  z = x ** y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    ** b  ! { dg-error "(A|a)ssumed.rank" }
  z = x ** i  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = a ** i  ! { dg-error "(A|a)ssumed.rank" }
  z = i ** y  ! OK
  c &  ! { dg-error "(A|a)ssumed.rank" }
    = i ** b  ! { dg-error "(A|a)ssumed.rank" }

  ! Comparisons

  r = x .eq. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .eq. b  ! { dg-error "(A|a)ssumed.rank" }
  r = x .eq. i  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a .eq. i  ! { dg-error "(A|a)ssumed.rank" }
  r = i .eq. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = i .eq. b  ! { dg-error "(A|a)ssumed.rank" }

  r = x .ne. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .ne. b  ! { dg-error "(A|a)ssumed.rank" }
  r = x .ne. i  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a .ne. i  ! { dg-error "(A|a)ssumed.rank" }
  r = i .ne. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = i .ne. b  ! { dg-error "(A|a)ssumed.rank" }

  r = x .lt. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .lt. b  ! { dg-error "(A|a)ssumed.rank" }
  r = x .lt. i  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a .lt. i  ! { dg-error "(A|a)ssumed.rank" }
  r = i .lt. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = i .lt. b  ! { dg-error "(A|a)ssumed.rank" }

  r = x .le. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .le. b  ! { dg-error "(A|a)ssumed.rank" }
  r = x .le. i  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a .le. i  ! { dg-error "(A|a)ssumed.rank" }
  r = i .le. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = i .le. b  ! { dg-error "(A|a)ssumed.rank" }

  r = x .gt. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .gt. b  ! { dg-error "(A|a)ssumed.rank" }
  r = x .gt. i  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a .gt. i  ! { dg-error "(A|a)ssumed.rank" }
  r = i .gt. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = i .gt. b  ! { dg-error "(A|a)ssumed.rank" }

  r = x .ge. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .ge. b  ! { dg-error "(A|a)ssumed.rank" }
  r = x .ge. i  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = a .ge. i  ! { dg-error "(A|a)ssumed.rank" }
  r = i .ge. y  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = i .ge. b  ! { dg-error "(A|a)ssumed.rank" }

  ! Logical operators

  r = .not. p  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = .not. l  ! { dg-error "(A|a)ssumed.rank" }
  r = .not. j  ! OK
  n = .not. j  ! { dg-error "(A|a)ssumed.rank" }

  r = p .and. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .and. m  ! { dg-error "(A|a)ssumed.rank" }
  r = p .and. j  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l .and. j  ! { dg-error "(A|a)ssumed.rank" }
  r = j .and. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = j .and. m  ! { dg-error "(A|a)ssumed.rank" }

  r = p .or. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .or. m  ! { dg-error "(A|a)ssumed.rank" }
  r = p .or. j  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l .or. j  ! { dg-error "(A|a)ssumed.rank" }
  r = j .or. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = j .or. m  ! { dg-error "(A|a)ssumed.rank" }

  r = p .eqv. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .eqv. m  ! { dg-error "(A|a)ssumed.rank" }
  r = p .eqv. j  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l .eqv. j  ! { dg-error "(A|a)ssumed.rank" }
  r = j .eqv. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = j .eqv. m  ! { dg-error "(A|a)ssumed.rank" }

  r = p .neqv. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l &  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    .neqv. m  ! { dg-error "(A|a)ssumed.rank" }
  r = p .neqv. j  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = l .neqv. j  ! { dg-error "(A|a)ssumed.rank" }
  r = j .neqv. q  ! OK
  n &  ! { dg-error "(A|a)ssumed.rank" }
    = j .neqv. m  ! { dg-error "(A|a)ssumed.rank" }

end subroutine  

! Check that calls to disallowed intrinsic functions produce a diagnostic.
! There are 100+ "elemental" intrinsics defined in the standard, and
! 25+ "transformational" intrinsics that accept array operands, and that
! doesn't include intrinsics in the standard modules.  To keep the length of
! this test to something sane, check only a handful of these functions on 
! the theory that related functions are probably implemented similarly and 
! probably share the same argument-processing code.  

subroutine test_intrinsics (i1, i2, r1, r2, c1, c2, l1, l2, s1, s2)
  implicit none
  integer :: i1(..), i2(..)
  real :: r1(..), r2(..)
  complex :: c1(..), c2(..)
  logical :: l1(..), l2(..)
  character :: s1(..), s2(..)

  integer :: i
  real :: r
  logical :: l

  ! trig, hyperbolic, other math functions
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = atan2 (r1, &  ! { dg-error "(A|a)ssumed.rank" }
             r2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = atan (r2)  ! { dg-error "(A|a)ssumed.rank" }
  c1 &  ! { dg-error "(A|a)ssumed.rank" }
    = atan (c2)  ! { dg-error "(A|a)ssumed.rank" }
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = cos (r2)  ! { dg-error "(A|a)ssumed.rank" }
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = exp (r2)  ! { dg-error "(A|a)ssumed.rank" }
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = sinh (r2)  ! { dg-error "(A|a)ssumed.rank" }
  
  ! bit operations
  l1 &  ! { dg-error "(A|a)ssumed.rank" }
    = blt (i1, &  ! { dg-error "(A|a)ssumed.rank" }
           i2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  l1 &  ! { dg-error "(A|a)ssumed.rank" }
    = btest (i1, 0)  ! { dg-error "(A|a)ssumed.rank" }
  i1 &  ! { dg-error "(A|a)ssumed.rank" }
    = not (i2)  ! { dg-error "(A|a)ssumed.rank" }
  i1 &  ! { dg-error "(A|a)ssumed.rank" }
    = popcnt (i2)  ! { dg-error "(A|a)ssumed.rank" }

  ! type conversions
  s1 &  ! { dg-error "(A|a)ssumed.rank" }
    = char (i1)  ! { dg-error "(A|a)ssumed.rank" }
  c1 &  ! { dg-error "(A|a)ssumed.rank" }
    = cmplx (r1, &  ! { dg-error "(A|a)ssumed.rank" }
             r2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  i1 &  ! { dg-error "(A|a)ssumed.rank" }
    = floor (r1)  ! { dg-error "(A|a)ssumed.rank" }
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = real (c1)  ! { dg-error "(A|a)ssumed.rank" }

  ! reductions
  l = any (l2)  ! { dg-error "(A|a)ssumed.rank" }
  r = dot_product (r1, &  ! { dg-error "(A|a)ssumed.rank" }
                   r2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  i = iall (i2, &  ! { dg-error "(A|a)ssumed.rank" }
            l2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }

  ! string operations
  s1 &  ! { dg-error "(A|a)ssumed.rank" }
    = adjustr (s2)  ! { dg-error "(A|a)ssumed.rank" }
  i1 &  ! { dg-error "(A|a)ssumed.rank" }
    = index (c1, &  ! { dg-error "(A|a)ssumed.rank" }
             c2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }

  ! misc
  i1 &  ! { dg-error "(A|a)ssumed.rank" }
    = cshift (i2, 4)  ! { dg-error "(A|a)ssumed.rank" }
  i = findloc (r1, 0.0)  ! { dg-error "(A|a)ssumed.rank" }
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = matmul (r1, &  ! { dg-error "(A|a)ssumed.rank" }
              r2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  r1 &  ! { dg-error "(A|a)ssumed.rank" }
    = reshape (r2, [10, 3])  ! { dg-error "(A|a)ssumed.rank" }
  i1 &  ! { dg-error "(A|a)ssumed.rank" }
    = sign (i1, &  ! { dg-error "(A|a)ssumed.rank" }
            i2)  ! { dg-error "(A|a)ssumed.rank" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
  s1 &  ! { dg-error "(A|a)ssumed.rank" }
    = transpose (s2)  ! { dg-error "(A|a)ssumed.rank" }

end subroutine
