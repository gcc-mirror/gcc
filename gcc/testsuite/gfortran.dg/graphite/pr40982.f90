! { dg-options "-O3 -fgraphite-identity -floop-interchange " }

module mqc_m


implicit none

private
public :: mutual_ind_quad_cir_coil

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal
real (kind = longreal), parameter, private :: small = 1.0e-10_longreal

contains

      subroutine mutual_ind_quad_cir_coil (r_coil, x_coil, y_coil, z_coil, h_coil, n_coil,  &
                                                      rotate_coil, m, mu, l12)
      real (kind = longreal), intent(in) :: r_coil, x_coil, y_coil, z_coil, h_coil, n_coil, &
                                            mu
      real (kind = longreal), dimension(:,:), intent(in) :: rotate_coil
      integer, intent(in) :: m
      real (kind = longreal), intent(out) :: l12
      real (kind = longreal), dimension(3,3) :: rotate_quad
      real (kind = longreal), dimension(9), save :: x2gauss, y2gauss, w2gauss, z1gauss,     &
                                                    w1gauss
      real (kind = longreal) :: xxvec, xyvec, xzvec, yxvec, yyvec, yzvec, zxvec, zyvec,     &
                                zzvec, magnitude, l12_lower, l12_upper, dx, dy, dz, theta,  &
                                a, b1, b2, numerator, denominator, coefficient, angle
      real (kind = longreal), dimension(3) :: c_vector, q_vector, rot_c_vector,             &
                                              rot_q_vector, current_vector,                 &
                                              coil_current_vec, coil_tmp_vector
      integer :: i, j, k
      logical, save :: first = .true.

      do i = 1, 2*m
          theta = pi*real(i,longreal)/real(m,longreal)
          c_vector(1) = r_coil * cos(theta)
          c_vector(2) = r_coil * sin(theta)
          coil_tmp_vector(1) = -sin(theta)
          coil_tmp_vector(2) = cos(theta)
          coil_tmp_vector(3) = 0.0_longreal
          coil_current_vec(1) = dot_product(rotate_coil(1,:),coil_tmp_vector(:))
          coil_current_vec(2) = dot_product(rotate_coil(2,:),coil_tmp_vector(:))
          coil_current_vec(3) = dot_product(rotate_coil(3,:),coil_tmp_vector(:))
          do j = 1, 9
              c_vector(3) = 0.5 * h_coil * z1gauss(j)
              rot_c_vector(1) = dot_product(rotate_coil(1,:),c_vector(:)) + dx
              rot_c_vector(2) = dot_product(rotate_coil(2,:),c_vector(:)) + dy
              rot_c_vector(3) = dot_product(rotate_coil(3,:),c_vector(:)) + dz
              do k = 1, 9
                  q_vector(1) = 0.5_longreal * a * (x2gauss(k) + 1.0_longreal)
                  q_vector(2) = 0.5_longreal * b1 * (y2gauss(k) - 1.0_longreal)
                  q_vector(3) = 0.0_longreal
                  rot_q_vector(1) = dot_product(rotate_quad(1,:),q_vector(:))
                  rot_q_vector(2) = dot_product(rotate_quad(2,:),q_vector(:))
                  rot_q_vector(3) = dot_product(rotate_quad(3,:),q_vector(:))
                  numerator = w1gauss(j) * w2gauss(k) *                                     &
                                                 dot_product(coil_current_vec,current_vector)
                  denominator = sqrt(dot_product(rot_c_vector-rot_q_vector,                 &
                                                                  rot_c_vector-rot_q_vector))
                  l12_lower = l12_lower + numerator/denominator
              end do
          end do
      end do
      l12 = coefficient * (b1 * l12_lower + b2 * l12_upper)
      end subroutine mutual_ind_quad_cir_coil

end module mqc_m
