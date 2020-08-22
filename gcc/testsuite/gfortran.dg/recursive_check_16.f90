! { dg-do  run }
! ! { dg-options "-fcheck=recursion" }
! PR 95743 - this used cause a runtime error.
! Test case by Antoine Lemoine

program test_recursive_call
   implicit none

   type t_tree_node
      type(t_tree_node), dimension(:), allocatable :: child
   end type

   type t_tree
      type(t_tree_node), allocatable :: root
   end type

   type(t_tree), allocatable :: tree

   allocate(tree)
   allocate(tree%root)
   allocate(tree%root%child(1))
   ! If the line below is removed, the code works fine.
   allocate(tree%root%child(1)%child(1))
   deallocate(tree)
end program
