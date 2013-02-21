! { dg-do compile }
!
! PR 56385: [4.6/4.7/4.8 Regression] [OOP] ICE with allocatable function result in a procedure-pointer component
!
! Contributed by Vladimir Fuka <vladimir.fuka@gmail.com>

  implicit none
  
  type :: TGeometricShape
  end type

  type :: TVolumeSourceBody
    class(TGeometricShape), allocatable :: GeometricShape
    procedure(scalar_flux_interface), pointer :: get_scalar_flux
  end type

  abstract interface
    function scalar_flux_interface(self) result(res)
      import
      real, allocatable :: res(:)
      class(TVolumeSourceBody), intent(in) :: self
    end function
  end interface

end
