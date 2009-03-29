! { dg-do run }
! Test fix for PR fortran/38823.
program power

   implicit none

   integer, parameter :: &
   &  s = kind(1.e0), &
   &  d = kind(1.d0), &
   &  e = max(selected_real_kind(precision(1.d0)+1), d)

  real(s),    parameter :: ris = 2.e0_s**2
  real(d),    parameter :: rid = 2.e0_d**2
  real(e),    parameter :: rie = 2.e0_e**2 
  complex(s), parameter :: cis = (2.e0_s,1.e0_s)**2
  complex(d), parameter :: cid = (2.e0_d,1.e0_d)**2
  complex(e), parameter :: cie = (2.e0_e,1.e0_e)**2

  real(s),    parameter :: rrs = 2.e0_s**2.e0
  real(d),    parameter :: rrd = 2.e0_d**2.e0
  real(e),    parameter :: rre = 2.e0_e**2.e0
  complex(s), parameter :: crs = (2.e0_s,1.e0_s)**2.e0
  complex(d), parameter :: crd = (2.e0_d,1.e0_d)**2.e0
  complex(e), parameter :: cre = (2.e0_e,1.e0_e)**2.e0

  real(s),    parameter :: rds = 2.e0_s**2.e0_d
  real(d),    parameter :: rdd = 2.e0_d**2.e0_d
  real(e),    parameter :: rde = 2.e0_e**2.e0_d
  complex(s), parameter :: cds = (2.e0_s,1.e0_s)**2.e0_d
  complex(d), parameter :: cdd = (2.e0_d,1.e0_d)**2.e0_d
  complex(e), parameter :: cde = (2.e0_e,1.e0_e)**2.e0_d

  real(s), parameter :: eps_s = 1.e-5_s
  real(d), parameter :: eps_d = 1.e-10_d
  real(e), parameter :: eps_e = 1.e-10_e

  if (abs(ris - 4) > eps_s) call abort
  if (abs(rid - 4) > eps_d) call abort
  if (abs(rie - 4) > eps_e) call abort
  if (abs(real(cis, s) - 3) > eps_s .or. abs(aimag(cis) - 4) > eps_s) call abort
  if (abs(real(cid, d) - 3) > eps_d .or. abs(aimag(cid) - 4) > eps_d) call abort
  if (abs(real(cie, e) - 3) > eps_e .or. abs(aimag(cie) - 4) > eps_e) call abort

  if (abs(rrs - 4) > eps_s) call abort
  if (abs(rrd - 4) > eps_d) call abort
  if (abs(rre - 4) > eps_e) call abort
  if (abs(real(crs, s) - 3) > eps_s .or. abs(aimag(crs) - 4) > eps_s) call abort
  if (abs(real(crd, d) - 3) > eps_d .or. abs(aimag(crd) - 4) > eps_d) call abort
  if (abs(real(cre, e) - 3) > eps_e .or. abs(aimag(cre) - 4) > eps_e) call abort

  if (abs(rds - 4) > eps_s) call abort
  if (abs(rdd - 4) > eps_d) call abort
  if (abs(rde - 4) > eps_e) call abort
  if (abs(real(cds, s) - 3) > eps_s .or. abs(aimag(cds) - 4) > eps_s) call abort
  if (abs(real(cdd, d) - 3) > eps_d .or. abs(aimag(cdd) - 4) > eps_d) call abort
  if (abs(real(cde, e) - 3) > eps_e .or. abs(aimag(cde) - 4) > eps_e) call abort

end program power
