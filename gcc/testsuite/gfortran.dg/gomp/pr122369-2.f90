! { dg-do compile }
! { dg-additional-options "-Wunused-label" }

! Check that a statement label that ends a loop in the first statement past a
! metadirective body is bound to the outer region.

implicit none
integer :: i, j
logical :: cond1, cond2
integer :: A(0:10,0:5), B(0:10,0:5)

cond1 = .true.
cond2 = .true.

!$omp metadirective when(user={condition(cond1)} : parallel do collapse(2))
      do 50 j = 0, 5
!$omp  metadirective when(user={condition(.false.)} : simd)
        do 51 i = 0, 10
          A(i,j) = i*10 + j
   51   continue
   50 continue

   do 55 i = 0, 5
   55 continue

!$omp begin metadirective when(user={condition(cond2)} : parallel do collapse(2))
      do 60 j = 0, 5
!$omp  metadirective when(user={condition(.false.)} : simd)
        do 61 i = 0, 10
          B(i,j) = i*10 + j
   61   continue
   60 continue
!$omp end metadirective

      do 70 j = 0, 5
      70 continue
end
