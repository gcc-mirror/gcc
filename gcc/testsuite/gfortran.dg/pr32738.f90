! { dg-do compile }
! PR fortran/32738 
!
! A regression that mysteriously appeared and disappeared again.
! Added to the testsuite "just in case".
!
! Contributed by Michael Richmond <michael DOT a DOT richmond AT nasa DT gov>
!

module cluster_definition
  implicit none
  integer, parameter, public:: cluster_size = 1000
end module cluster_definition
module cluster_tree
  use cluster_definition, only: ct_cluster_size => cluster_size
  implicit none
  private
  private:: ct_initialize, ct_dealloc, ct_tree_size
  public:: initialize, dealloc, tree_size
  interface initialize
     module procedure ct_initialize
  end interface
  interface dealloc
     module procedure ct_dealloc
  end interface
  interface tree_size
     module procedure ct_tree_size
  end interface
contains
  subroutine ct_initialize()
  end subroutine ct_initialize
  subroutine ct_dealloc()
  end subroutine ct_dealloc
  function ct_tree_size(t) result(s)
    integer :: t
    integer :: s
    s = 0
  end function ct_tree_size
end module cluster_tree
program example
  use cluster_tree
  implicit none
     print *, tree_size(1)
end program example

! { dg-final { cleanup-modules "cluster_definition cluster_tree" } }
