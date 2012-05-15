! { dg-do compile }
! Tests the fix for PR37274 comment 4 in which the use associated 'vector' was
! passed up from the interface to the module 'tools_math'.
!
! Contributed by Mikael Morin  <mikael.morin@tele2.fr>
!
module class_vector
  implicit none
  type vector
  end type vector
end module class_vector

module tools_math
  implicit none
  interface lin_interp
     function lin_interp_v()
       use class_vector
       type(vector) :: lin_interp_v
     end function lin_interp_v
  end interface
end module tools_math

module smooth_mesh
  use tools_math
  implicit none
  type(vector ) :: new_pos  ! { dg-error "used before it is defined" }
end module smooth_mesh

