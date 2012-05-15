! { dg-do compile }
!
! PR 44213: ICE when extending abstract type
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module ice_module
  type :: a_type
  end type a_type

  type,extends(a_type),abstract :: b_type
  end type b_type

  type,extends(b_type) :: c_type
  end type c_type
end module ice_module
 
