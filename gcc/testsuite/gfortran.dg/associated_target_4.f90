! { dg-do run }
!
! PR 44696: [OOP] ASSOCIATED fails on polymorphic variables
!
! Original test case by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>
! Modified by Janus Weil <janus@gcc.gnu.org>

program rte1
  implicit none
  type::node_type
     class(node_type),pointer::parent,child
     integer::id
  end type node_type
  class(node_type),pointer::root
  allocate(root)
  allocate(root%child)
  root%child%parent=>root
  root%id=1
  root%child%id=2
  print *,root%child%id," is child of ",root%id,":"
  print *,root%child%parent%id,root%id
  if (.not. associated(root%child%parent,root)) call abort()
end program rte1
