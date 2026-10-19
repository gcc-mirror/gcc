! { dg-do run }
! See attachment to PR82173 comment 10 - reproducers/bug6-alloc-self-component-ice.f90
program g2
  type :: node(n)
    integer, len :: n = 1
    real :: v(n)
    type(node(:)), allocatable :: owned
  end type
  type(node(3)) :: head
  head%v = 1.0
  if (allocated (head%owned)) stop 1
end program  ! Seg fault at trans-decl.cc:1697
