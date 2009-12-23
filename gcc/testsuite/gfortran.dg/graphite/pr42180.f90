module mcc_m
  integer, parameter, private :: longreal = selected_real_kind(15,90)
contains
  subroutine mutual_ind_cir_cir_coils (m, l12)
    real (kind = longreal), intent(out) :: l12
    real (kind = longreal), dimension(1:9), save :: zw
    gauss:do i = 1, 9
       theta_l12 = 0.0_longreal
       theta1:   do n1 = 1, 2*m
          theta_1 = pi*real(n1,longreal)/real(m,longreal)
          theta2:       do n2 = 1, 2*m
             numerator = -sin(theta_1)*tvx + cos(theta_1)*tvy
             theta_l12 = theta_l12 + numerator/denominator
          end do theta2
       end do theta1
       l12 = l12 + zw(i)*theta_l12
    end do gauss
    l12 = coefficient * l12
  end subroutine mutual_ind_cir_cir_coils
end module mcc_m
