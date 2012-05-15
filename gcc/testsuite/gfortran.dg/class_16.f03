! { dg-do compile }
!
! PR 43896: [fortran-dev Regression] ICE in gfc_conv_variable, at fortran/trans-expr.c:551
!
! Contributed by Fran Martinez Fadrique <fmartinez@gmv.com>

module m_rotation_matrix

  type t_rotation_matrix
    contains
      procedure :: array => rotation_matrix_array
  end type

contains

  function rotation_matrix_array( rot ) result(array)
    class(t_rotation_matrix) :: rot
    double precision, dimension(3,3)    :: array
  end function

end module
