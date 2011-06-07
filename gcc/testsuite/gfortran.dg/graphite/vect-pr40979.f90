! { dg-do compile }
! { dg-require-effective-target vect_double }
! { dg-additional-options "-msse2" { target { { i?86-*-* x86_64-*-* } && ilp32 } } }

module mqc_m
integer, parameter, private :: longreal = selected_real_kind(15,90)
contains
      subroutine mutual_ind_quad_cir_coil (m, l12)
      real (kind = longreal), dimension(9), save :: w2gauss, w1gauss
      real (kind = longreal) :: l12_lower, num, l12
      real (kind = longreal), dimension(3) :: current, coil
      w2gauss(1) = 16.0_longreal/81.0_longreal
      w1gauss(5) = 0.3302393550_longreal
      do i = 1, 2*m
          do j = 1, 9
              do k = 1, 9
                  num = w1gauss(j) * w2gauss(k) * dot_product(coil,current)
                  l12_lower = l12_lower + num
              end do
          end do
      end do
      l12 = l12_lower
      end subroutine mutual_ind_quad_cir_coil
end module mqc_m

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } }
! { dg-final { cleanup-tree-dump "vect" } }
