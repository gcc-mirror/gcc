! { dg-do run }
! { dg-options "-std=gnu" }
!
! 13.7.53    INT(A [, KIND])
!
! Description.  Convert to integer type.
! Class.       Elemental function.
! Arguments.
!    A               shall be of type integer, real, or complex,
!                    or a boz-literal-constant .
!    KIND (optional) shall be a scalar integer initialization expression.
!
! Result Characteristics. Integer. If KIND is present, the kind type
!    parameter is that specified by the value of KIND; otherwise, the
!    kind type parameter is that of default integer type.
!
! Result Value.
!
!    Case (1):  If A is of type integer, INT (A) = A.
!
!    Case (2):  If A is of type real, there are two cases:
!      (a) if |A| < 1, INT (A) has the value 0
!      (b) if |A| .ge. 1, INT (A) is the integer whose magnitude is the
!          largest integer that does not exceed the magnitude of A and
!          whose sign is the same as the sign of A.
!
!    Case (3):  If A is of type complex, INT(A) = INT(REAL(A, KIND(A))).
!
!    Case (4):  If A is a boz-literal-constant, it is treated as if it were
!               an int-literal-constant with a kind-param that specifies the
!               representation method with the largest decimal exponent range
!               supported by the processor.
!
!    Example. INT (­3.7) has the value ­3.
!
module mykinds
   integer, parameter :: ik1 = selected_int_kind(2)
   integer, parameter :: ik2 = selected_int_kind(4)
   integer, parameter :: ik4 = selected_int_kind(9)
   integer, parameter :: ik8 = selected_int_kind(18)
   integer, parameter :: sp = selected_real_kind(6,30)
   integer, parameter :: dp = selected_real_kind(15,300)
   integer, parameter :: ck = kind('a')
end module mykinds

program test_int

   use mykinds

   integer(ik1) i1
   integer(ik2) i2
   integer(ik4) i4
   integer(ik8) i8
   real(sp) r4
   real(dp) r8
   complex(sp) c4
   complex(dp) c8
   !
   ! Case 1
   !
   i1 = int(-3)
   i2 = int(-3)
   i4 = int(-3)
   i8 = int(-3)
   if (i1 /= -3_ik1 .or. i2 /= -3_ik2) call abort
   if (i4 /= -3_ik4 .or. i8 /= -3_ik8) call abort

   i1 = int(5,  ik1)
   i2 = int(i1, ik2)
   i4 = int(i1, ik4)
   i8 = int(i1, ik8)
   if (i1 /= 5_ik1 .or. i2 /= 5_ik2) call abort
   if (i4 /= 5_ik4 .or. i8 /= 5_ik8) call abort

   i8 = int(10, ik8)
   i1 = int(i8, ik1)
   i2 = int(i8, ik2)
   i4 = int(i8, ik4)
   if (i1 /= 10_ik1 .or. i2 /= 10_ik2) call abort
   if (i4 /= 10_ik4 .or. i8 /= 10_ik8) call abort
   !
   ! case 2(b)
   !
   r4 = -3.7_sp
   i1 = int(r4,  ik1)
   i2 = int(r4, ik2)
   i4 = int(r4, ik4)
   i8 = int(r4, ik8)
   if (i1 /= -3_ik1 .or. i2 /= -3_ik2) call abort
   if (i4 /= -3_ik4 .or. i8 /= -3_ik8) call abort

   r8 = -3.7_dp
   i1 = int(r8,  ik1)
   i2 = int(r8, ik2)
   i4 = int(r8, ik4)
   i8 = int(r8, ik8)
   if (i1 /= -3_ik1 .or. i2 /= -3_ik2) call abort
   if (i4 /= -3_ik4 .or. i8 /= -3_ik8) call abort
   !
   ! Case 2(a)
   !
   r4 = -3.7E-1_sp
   i1 = int(r4, ik1)
   i2 = int(r4, ik2)
   i4 = int(r4, ik4)
   i8 = int(r4, ik8)
   if (i1 /= 0_ik1 .or. i2 /= 0_ik2) call abort
   if (i4 /= 0_ik4 .or. i8 /= 0_ik8) call abort

   r8 = -3.7E-1_dp
   i1 = int(r8, ik1)
   i2 = int(r8, ik2)
   i4 = int(r8, ik4)
   i8 = int(r8, ik8)
   if (i1 /= 0_ik1 .or. i2 /= 0_ik2) call abort
   if (i4 /= 0_ik4 .or. i8 /= 0_ik8) call abort
   !
   ! Case 3
   !
   c4 = (-3.7E-1_sp,3.7E-1_sp)
   i1 = int(c4, ik1)
   i2 = int(c4, ik2)
   i4 = int(c4, ik4)
   i8 = int(c4, ik8)
   if (i1 /= 0_ik1 .or. i2 /= 0_ik2) call abort
   if (i4 /= 0_ik4 .or. i8 /= 0_ik8) call abort

   c8 = (-3.7E-1_dp,3.7E-1_dp)
   i1 = int(c8, ik1)
   i2 = int(c8, ik2)
   i4 = int(c8, ik4)
   i8 = int(c8, ik8)
   if (i1 /= 0_ik1 .or. i2 /= 0_ik2) call abort
   if (i4 /= 0_ik4 .or. i8 /= 0_ik8) call abort

   c4 = (-3.7_sp,3.7_sp)
   i1 = int(c4, ik1)
   i2 = int(c4, ik2)
   i4 = int(c4, ik4)
   i8 = int(c4, ik8)
   if (i1 /= -3_ik1 .or. i2 /= -3_ik2) call abort
   if (i4 /= -3_ik4 .or. i8 /= -3_ik8) call abort

   c8 = (3.7_dp,3.7_dp)
   i1 = int(c8, ik1)
   i2 = int(c8, ik2)
   i4 = int(c8, ik4)
   i8 = int(c8, ik8)
   if (i1 /= 3_ik1 .or. i2 /= 3_ik2) call abort
   if (i4 /= 3_ik4 .or. i8 /= 3_ik8) call abort
   !
   ! Case 4
   !
   i1 = int(b'0011', ik1)
   i2 = int(b'0011', ik2)
   i4 = int(b'0011', ik4)
   i8 = int(b'0011', ik8)
   if (i1 /= 3_ik1 .or. i2 /= 3_ik2) call abort
   if (i4 /= 3_ik4 .or. i8 /= 3_ik8) call abort
   i1 = int(o'0011', ik1)
   i2 = int(o'0011', ik2)
   i4 = int(o'0011', ik4)
   i8 = int(o'0011', ik8)
   if (i1 /= 9_ik1 .or. i2 /= 9_ik2) call abort
   if (i4 /= 9_ik4 .or. i8 /= 9_ik8) call abort
   i1 = int(z'0011', ik1)
   i2 = int(z'0011', ik2)
   i4 = int(z'0011', ik4)
   i8 = int(z'0011', ik8)
   if (i1 /= 17_ik1 .or. i2 /= 17_ik2) call abort
   if (i4 /= 17_ik4 .or. i8 /= 17_ik8) call abort
   
end program test_int

! { dg-final { cleanup-modules "mykinds" } }
