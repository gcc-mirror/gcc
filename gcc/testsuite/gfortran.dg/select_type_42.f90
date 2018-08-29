! { dg-do run }
!
! Tests the fix for PR82275.
! Associating a name with a reduced-dimension section of a
! multidimensional array precluded subsequent use of the name
! with the appropriately reduced dimensionality and instead
! required use of the (invalid) full set of original dimensions.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  type component
   integer :: i
  end type
  type container
    class(component), allocatable :: component_array(:,:)
  end type
  type(container) bag
  type(component) section_copy
  allocate(bag%component_array, source = reshape ([component(10), component (100)], [1,2]))
  select type(associate_name=>bag%component_array(1,:))
    type is (component)
      section_copy = associate_name(2)  ! gfortran rejected valid
!      section_copy = associate_name(1,1)! gfortran accepted invalid
  end select
  if (section_copy%i .ne. 100) stop 1
end
