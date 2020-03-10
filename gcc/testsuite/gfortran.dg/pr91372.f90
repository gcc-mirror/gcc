! { dg-do compile }
! { dg-require-visibility "" }
! PR fortran/91372   
module module_sf_lake
    implicit none 
    integer, parameter ::      r8 = selected_real_kind(12) 
    integer, private  :: i
    real(r8) :: sand(2)                           ! percent sand
    data(sand(i), i=1,2)/92.,80./
end module module_sf_lake
