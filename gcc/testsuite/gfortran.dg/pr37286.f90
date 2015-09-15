! { dg-do compile }
! { dg-require-visibility "" }

module general_rand
  implicit none
  private

  integer, public, parameter :: GNDP = kind(1.0d0)

  real(kind = GNDP), save :: &
    gnc = 362436.0 / 16777216.0, &
    gncd = 7654321.0 / 16777216.0, &
    gncm = 16777213.0 / 16777216.0
  integer, save :: &
    gni97 = 97, &
    gnj97 = 33

  real(kind = GNDP), save :: gnu(97)

contains
  subroutine gn_fatal(message)
    character(len = *), intent(in) :: message

    stop 1 
  end subroutine gn_fatal

  function gn_monte_rand(min, max) result(monte)
    real(kind = GNDP), intent(in) :: min 
    real(kind = GNDP), intent(in) :: max
    real(kind = GNDP) :: monte

    real :: monte_temp

    if (min > max) then
      call gn_fatal('gn_monte_rand: min > max')
    else if (min == max) then
      call gn_fatal('gn_monte_rand: min = max: returning min')
      monte_temp = min
    else

      monte_temp = gnu(gni97) - gnu(gnj97)
      if (monte_temp < 0.0) then
        monte_temp = monte_temp + 1.0
      end if

      gnu(gni97) = monte_temp
      gni97 = gni97 - 1
      if (gni97 == 0) then
        gni97 = 97
      end if
    end if

    monte = min + monte_temp * (max - min)

  end function gn_monte_rand

end module general_rand
