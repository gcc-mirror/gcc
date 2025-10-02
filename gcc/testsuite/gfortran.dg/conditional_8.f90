! { dg-do run }
! { dg-options "-std=f2023" }
implicit none
integer :: aa(2)
aa = [1, 2]

print *, (aa(1) > 0 ? aa(2) : g())
contains
integer function g()
  allocatable :: g
  error stop "should not be called"
  g = 3
end
end
