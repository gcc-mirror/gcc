! { dg-do compile }
! testcase from PR 17583
module bidon 
 
 interface 
  subroutine drivexc(nspden,rho_updn) 
   integer,  intent(in) :: nspden 
   integer, intent(in) :: rho_updn(nspden) 
  end subroutine drivexc 
 end interface 
 
end module bidon 
 
 subroutine nonlinear(nspden) 
 
 use bidon 
  
 integer,intent(in) :: nspden 
 
 end subroutine nonlinear

! { dg-final { cleanup-modules "bidon" } }
