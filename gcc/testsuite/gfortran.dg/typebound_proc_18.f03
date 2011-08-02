! { dg-do compile }
!
! PR 45456: [4.6 Regression] [OOP] Bogus pointer initialization error on pointer-valued TBP
!
! Contributed by Andrew Benson <abenson@its.caltech.edu>

module Merger_Trees
  private
  public :: mergerTree

  type mergerTree
   contains
     procedure :: getNode => Tree_Node_Get
  end type mergerTree

contains

  function Tree_Node_Get(thisTree,nodeIndex) result(foundNode)
    implicit none
    class(mergerTree), intent(inout) :: thisTree
    integer,           intent(in)    :: nodeIndex
    integer,           pointer       :: foundNode

    return
  end function Tree_Node_Get

end module Merger_Trees

! { dg-final { cleanup-modules "merger_trees" } }
