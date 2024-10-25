! { dg-do compile }
!
! Test the fix for pr79685, which failed as in the comments below.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module omega_color
  implicit none

  type omega_color_factor
     integer :: i
  end type

  type(omega_color_factor), parameter :: op = omega_color_factor (199)

end module

module foo
  use omega_color, ocf => omega_color_factor, ocfp => op
  implicit none

  type(ocf) :: table_color_factors1 = ocf(42)
  type(ocf) :: table_color_factors2
  type(ocf) :: table_color_factors3 (2)
  type(ocf) :: table_color_factors4
  data table_color_factors2 / ocf(99) /        ! This failed in gfc_match_structure_constructor.
  data table_color_factors3 / ocf(1), ocf(2) / ! ditto.
  data table_color_factors4 / ocfp /
end module

  use foo
  if (table_color_factors1%i .ne. 42) stop 1
  if (table_color_factors2%i .ne. 99) stop 2
  if (any (table_color_factors3%i .ne. [1,2])) stop 3
  if (table_color_factors4%i .ne. 199) stop 4
end

