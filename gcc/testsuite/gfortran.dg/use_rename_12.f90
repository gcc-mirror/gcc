! { dg-do compile }
! PR fortran/116530 - ICE with member of namelist renamed by use module
!
! Reported by philippe.wautelet at cnrs.fr 

module mod_nml1
  implicit none
  logical :: ldiag
  namelist /nam_nml1/ldiag
end module mod_nml1

module mod_interm
  use mod_nml1
end module mod_interm

program ice_nml
  use mod_nml1,        ldiag_nml1 => ldiag
  use mod_nml1, only : ldiag_only => ldiag
  use mod_interm
  implicit none
  integer :: ilu = 10
  read(unit=ilu,nml=nam_nml1)
  write(unit=*,nml=nam_nml1)
  print *, ldiag
  print *, ldiag_nml1
  print *, ldiag_only
end program ice_nml
