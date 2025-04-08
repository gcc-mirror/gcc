! { dg-do run }
! { dg-additional-options "-fcheck=bounds" }
!
! PR fortran/116706 - bogus bounds check for reallocation on assignment
! Contributed by Balint Aradi  <baradi09 at gmail.com>

program testprog
  implicit none

  type :: data_node
     integer, allocatable :: data(:)
  end type data_node

  type :: data_list
     type(data_node), pointer :: nodes(:) => null()
  end type data_list

  type :: upoly_node
     class(*), allocatable :: data(:)
  end type upoly_node

  type :: star_list
     type(upoly_node), pointer :: nodes(:) => null()
  end type star_list

  type(data_list) :: datalist
  type(star_list) :: starlist
  class(star_list), allocatable :: astarlist
  class(star_list), pointer     :: pstarlist

  allocate (datalist%nodes(2))
  datalist%nodes(1)%data = [1, 2, 3]

  allocate (starlist%nodes(2))
  starlist%nodes(1)%data = [1., 2., 3.]

  allocate (astarlist)
  allocate (astarlist%nodes(2))
  astarlist%nodes(1)%data = [1, 2, 3]

  allocate (pstarlist)
  allocate (pstarlist%nodes(2))
  pstarlist%nodes(1)%data = [1., 2., 3.]

end program testprog
