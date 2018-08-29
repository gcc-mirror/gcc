! { dg-do compile }
!
! Test the fix for PR83148.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module fhypre
  use iso_c_binding, only: c_ptr, c_null_ptr
  use iso_c_binding, only: hypre_obj => c_ptr, hypre_null_obj => c_null_ptr
  private
  public :: hypre_obj, hypre_null_obj
end module

module hypre_hybrid_type
  use fhypre
  type hypre_hybrid
    type(hypre_obj) :: solver = hypre_null_obj
  end type hypre_hybrid
end module

  use hypre_hybrid_type
  class(hypre_hybrid), allocatable :: x
  allocate (x)
end

