! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR64932.
!
! Reported by Daniel Shapiro  <shapero@uw.edu>
!
module coo_graphs
  implicit none
  type :: dynamic_array
    integer :: length, capacity, min_capacity
    integer, allocatable :: array(:)
  end type
  type :: coo_graph
    type(dynamic_array) :: edges(2)
    integer, private :: ne
  end type coo_graph
contains
  subroutine coo_dump_edges(g, edges)
    class(coo_graph), intent(in) :: g
    integer, intent(out) :: edges(:,:)
  end subroutine coo_dump_edges
end module coo_graphs
! { dg-final { scan-tree-dump-times "__builtin_free" 6 "original" } }
