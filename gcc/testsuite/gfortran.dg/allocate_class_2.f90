! { dg-do compile }
!
! PR 52552: [OOP] ICE when trying to allocate non-allocatable object giving a dynamic type
!
! Contributed by <gccbgz.lionm@xoxy.net>


  type t
    integer :: i
  end type
  
  class(t) :: o      ! { dg-error "must be dummy, allocatable or pointer" }

  allocate(t::o)     ! { dg-error "is neither a data pointer nor an allocatable variable" }

end 
