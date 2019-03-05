! PR middle-end/83977
! { dg-do compile }

integer function foo (a, b) ! { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } }
   integer :: a, b
!$omp declare simd uniform(b) linear(ref(a):b)
   a = a + 1
! This function can't be called from simd loops,
! because it violates declare simd restrictions.
! We shouldn't ICE on it though, nor attempt to generate
! simd clones for the *omp_fn* functions.
!$omp parallel
   call sub
!$omp end parallel
end
