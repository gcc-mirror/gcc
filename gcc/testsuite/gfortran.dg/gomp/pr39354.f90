! PR fortran/39354
! { dg-do compile }
! { dg-options "-fopenmp" }
      SUBROUTINE ltest(l1, l2, l3, l4, r1, r2, r3, r4)
      LOGICAL l1, l2, l3, l4, r1, r2, r3, r4
!$OMP ATOMIC
      l1 = l1 .and. r1
!$OMP ATOMIC
      l2 = l2 .or. r2
!$OMP ATOMIC
      l3 = l3 .eqv. r3
!$OMP ATOMIC
      l4 = l4 .neqv. r4
      END
      SUBROUTINE itest(l1, l2, l3, l4, l5, l6, l7, l8, l9, &
&                      r1, r2, r3, r4, r5, r6, r7, r8, r9)
      INTEGER l1, l2, l3, l4, l5, l6, l7, l8, l9, &
&             r1, r2, r3, r4, r5, r6, r7, r8, r9
!$OMP ATOMIC
      l1 = l1 + r1
!$OMP ATOMIC
      l2 = l2 - r2
!$OMP ATOMIC
      l3 = l3 * r3
!$OMP ATOMIC
      l4 = l4 / r4
!$OMP ATOMIC
      l5 = max (l5, r1, r5)
!$OMP ATOMIC
      l6 = min (r1, r6, l6)
!$OMP ATOMIC
      l7 = iand (l7, r7)
!$OMP ATOMIC
      l8 = ior (r8, l8)
!$OMP ATOMIC
      l9 = ieor (l9, r9)
      END
