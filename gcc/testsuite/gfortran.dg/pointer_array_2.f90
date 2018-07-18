! { dg-do compile }
!
! Test the fix for PR40737 as part of the overall fix for PR34640.
!
! Contributed by David Hough  <dh458@oakapple.net>
!
module testmod

integer, parameter :: standard_integer = 1
integer, parameter :: int = KIND( standard_integer)

integer, parameter :: i8  = selected_int_kind(12)
integer, parameter :: i4  = selected_int_kind(8)
integer, parameter :: i2  = selected_int_kind(4)

integer, parameter :: standard_real = 1.
integer, parameter :: std_real = KIND( standard_real)

integer, parameter :: r8  = selected_real_kind(12)
integer, parameter :: r4  = selected_real_kind(6)
integer, parameter :: double  = selected_real_kind(20)

integer, parameter :: name_string_length = 40
integer, parameter :: file_name_length = 60
integer, parameter :: text_string_length = 80
integer, parameter :: max_kwd_lgth = file_name_length

integer(int) :: bytes_per_int  = 4
integer(int) :: bytes_per_real = 8
integer(int) :: workcomm, spincomm

   integer(int), parameter :: nb_directions = 3,  &
                              direction_x = 1,    &
                              direction_y = 2,    &
                              direction_z = 3,    &
                              nb_ghost_cells = 5     ! might be different for the lagrange step?

   integer(int), parameter :: ends = 4,            &
                              lower_ghost = 1,     &
                              lower_interior = 2,  &
                              upper_interior = 3,  &
                              upper_ghost = 4

   ! Neighbors
   integer(int), parameter :: side = 2,       &
                              lower_end = 1,  &
                              upper_end = 2


   integer(int), parameter :: nb_variables = 5,    &
                              ro_var = 1,          &
                              ets_var = 2,         &
                              u_var = 3,           &
                              up1_var = 4,         &
                              up2_var = 5,         &
                              eis_var = 6,         &
                              ecs_var = 7,         &
                              p_var = 8,           &
                              c_var = 9,           &
                              nb_var_sortie = 9

   type :: VARIABLES_LIGNE
      sequence
      real, pointer, dimension( :, :) :: l
   end type VARIABLES_LIGNE

   type VARIABLES_MAILLE
      sequence
      real(r8), dimension( nb_variables) :: cell_var
   end type VARIABLES_MAILLE

   integer(int), dimension( nb_directions) :: &
         first_real_cell,    &  ! without ghost cells
         last_real_cell,     &  !
         nb_real_cells,      &  !
         first_work_cell,    &  ! including ghost cells
         last_work_cell,     &  !
         nb_work_cells,      &  !
         global_nb_cells        ! number of real cells, for the entire grid

   integer(int) :: dim_probleme  ! dimension du probleme (1, 2 ou 3)

   integer(int) :: largest_local_size   ! the largest of the 3 dimensions of the local grid

   ! Hydro variables of the actual domain
   ! There are 3 copies of these, for use according to current work direction
   type (VARIABLES_MAILLE), allocatable, target, dimension( :, :, :) ::  &
            Hydro_vars_XYZ,  &
            Hydro_vars_YZX,  &
            Hydro_vars_ZXY

   ! Pointers to current and next Hydro var arrays
   type (VARIABLES_MAILLE), pointer, dimension( :, :, :) :: Hydro_vars,      &
                                                            Hydro_vars_next

   ! Which of these 3 copies of the 3D arrays has been updated last
   integer(int) :: last_updated_3D_array = 0

   real(r8), pointer, dimension( :) ::        &
         ! Variables "permanentes" (entrant dans la projection)
         Ro,      & ! densite
         Ets,     & ! energie totale specifique
         Um,      & ! vitesse aux mailles, dans la direction de travail
         Xn,      & ! abscisse en fin de pas de temps
         ! Variables en lecture seulement
         Um_p1,   & ! vitesse aux mailles, dans les directions
         Um_p2,   & !                      orthogonales
         Xa,      & ! abscisses des noeuds en debut de pas de temps
         Dxa,     & ! longueur des mailles en debut de pas de temps
         U_dxa      ! inverses des longueurs des mailles

end module testmod


subroutine TF_AD_SPLITTING_DRIVER_PLANE

use testmod

implicit none
save

   real(r8), allocatable, dimension( :) ::  &
         ! Variables maille recalculees a chaque pas de temps
         Eis,     & ! energie interne specifique (seulement pour calculer la pression)
         Vit_son, & ! comme son nom l'indique
         C_f_l,   & ! nombre de Courant
         Pm,      & ! pression aux mailles
         ! Variables aux noeuds
         Un,      & ! vitesse des noeuds
         Pn         ! pression aux noeuds


integer(int) :: i, j, k
integer(int) :: first_cell, last_cell

         Ro => Hydro_vars( first_cell:last_cell, j, k)%cell_var( ro_var)
         Ets => Hydro_vars( first_cell:last_cell, j, k)%cell_var( ets_var)
         Um => Hydro_vars( first_cell:last_cell, j, k)%cell_var( u_var)
         Um_p1 => Hydro_vars( first_cell:last_cell, j, k)%cell_var( up1_var)
         Um_p2 => Hydro_vars( first_cell:last_cell, j, k)%cell_var( up2_var)

end subroutine TF_AD_SPLITTING_DRIVER_PLANE

