! { dg-do compile }
! Tests the fix for PR27709 in which the specification expression on
! line 22 was not resolved because of the multiple component references.
!
! Contributed by David Ham  <David@ham.dropbear.id.au>
!
module elements
  implicit none
  type element_type
     type(ele_numbering_type), pointer :: numbering
  end type element_type
  type ele_numbering_type
     integer, dimension(:,:), pointer :: number2count
  end type ele_numbering_type
end module elements
module global_numbering
  use elements
  implicit none
contains
  function element_local_coords(element) result (coords)
    type(element_type), intent(in) :: element    
    real, dimension(size(element%numbering%number2count, 1)) :: coords
    coords=0.0 
  end function element_local_coords
end module global_numbering

  use global_numbering
  type (element_type) :: e
  type (ele_numbering_type), target :: ent
  allocate (ent%number2count (2,2))
  e%numbering => ent
  print *, element_local_coords (e)
end
! { dg-final { cleanup-modules "elements global_numbering" } }
