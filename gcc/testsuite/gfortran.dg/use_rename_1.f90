! { dg-do compile }
! Tests the fix for PR34854, in which the second of the two subroutines would fail
! because the the type declaration of nmoltype_phase would incorrectly conflict
! with the type given to the module variable of the same name.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
module common_init_conf
  integer, dimension(2) :: Nmoltype_phase
end module common_init_conf

subroutine read_initial_config_nml1()
  use common_init_conf, nmoltype_phase_com => nmoltype_phase
  use common_init_conf
  implicit none
  integer :: nmoltype_phase
  namelist /confNmoltypePhase/ nmoltype_phase
end subroutine read_initial_config_nml1

subroutine read_initial_config_nml2()
  use common_init_conf
  use common_init_conf, nmoltype_phase_com => nmoltype_phase
  implicit none
  integer :: nmoltype_phase
  namelist /confNmoltypePhase/ nmoltype_phase
end subroutine read_initial_config_nml2
! { dg-final { cleanup-modules "common_init_conf" } }
