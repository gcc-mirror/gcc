! { dg-do compile }
!
! PR 44207: ICE with ALLOCATABLE components and SOURCE
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

program ice_prog

type::ice_type
  integer,dimension(:),allocatable::list
end type ice_type

type(ice_type)::this
integer::dim=10,i

allocate(this%list(dim),source=[(i,i=1,dim)])

end program ice_prog
