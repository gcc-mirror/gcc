! { dg-do compile }
!
! PR 49417: [4.6/4.7 Regression] [OOP] ICE on invalid CLASS component declaration
!
! Contributed by Andrew Benson <abenson@its.caltech.edu>

 type :: nodeWrapper
 end type nodeWrapper

 type, extends(nodeWrapper) :: treeNode
    class(nodeWrapper) :: subComponent   ! { dg-error "must be allocatable or pointer" }
 end type treeNode

end
