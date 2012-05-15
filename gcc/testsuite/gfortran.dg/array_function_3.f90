! { dg-do compile }

! PR fortran/36167
! This used to cause an ICE because of a missing array spec after interface
! mapping.

! Contributed by Frank Muldoon <fmuldoo@me.lsu.edu>

module communication_tools

contains   
!*******************************************************************************
function overlap_1(u,lbound_u,ubound_u)
!*******************************************************************************
integer, intent(in), dimension(:) :: lbound_u,ubound_u
real, intent(in), dimension(lbound_u(1):ubound_u(1),lbound_u(2):ubound_u(2),&
                            lbound_u(3):ubound_u(3)) :: u

real, dimension(&
lbound(u,1):ubound(u,1),&
lbound(u,2):ubound(u,2),&
lbound(u,3):ubound(u,3)) :: overlap_1

return
end function overlap_1

end module communication_tools

!*******************************************************************************
subroutine write_out_particles
!*******************************************************************************

use communication_tools
real, dimension(1:5, 2:4, 3:10) :: vorticityMag
real, allocatable, dimension(:,:,:) :: temp3d

allocate(temp3d( &
lbound(overlap_1(vorticityMag,lbound(vorticityMag),ubound(vorticityMag)),1):&
ubound(overlap_1(vorticityMag,lbound(vorticityMag),ubound(vorticityMag)),1),&
lbound(overlap_1(vorticityMag,lbound(vorticityMag),ubound(vorticityMag)),2):&
ubound(overlap_1(vorticityMag,lbound(vorticityMag),ubound(vorticityMag)),2),&
lbound(overlap_1(vorticityMag,lbound(vorticityMag),ubound(vorticityMag)),3):&
ubound(overlap_1(vorticityMag,lbound(vorticityMag),ubound(vorticityMag)),3)))

return 
end subroutine write_out_particles
