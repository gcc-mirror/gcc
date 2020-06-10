! { dg-do compile }

module ptrmod
contains
subroutine lengthX(x, i) ! { dg-error "Dummy 'x' at .1. cannot have an initializer" }
   implicit none
   real, pointer, intent(out) :: x(:)=>null()
   integer :: i
   x=>null()
   allocate(x(i))
   x=i
end subroutine
end module

