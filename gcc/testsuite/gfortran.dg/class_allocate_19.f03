! { dg-do run }
!
! Contributed by: Vladimir Fuka  <vladimir.fuka@gmail.com>

use iso_c_binding
implicit none
real, target :: e
class(*), allocatable, target :: a(:)
e = 1.0
call add_element_poly(a,e)
if (size(a) /= 1) call abort()
call add_element_poly(a,e)
if (size(a) /= 2) call abort()
select type (a)
  type is (real)
    if (any (a /= [ 1, 1])) call abort()
end select
contains
    subroutine add_element_poly(a,e)
      use iso_c_binding
      class(*),allocatable,intent(inout),target :: a(:)
      class(*),intent(in),target :: e
      class(*),allocatable,target :: tmp(:)
      type(c_ptr) :: dummy

      interface
        function memcpy(dest,src,n) bind(C,name="memcpy") result(res)
          import
          type(c_ptr) :: res
          integer(c_intptr_t),value :: dest
          integer(c_intptr_t),value :: src
          integer(c_size_t),value :: n
        end function
      end interface

      if (.not.allocated(a)) then
        allocate(a(1), source=e)
      else
        allocate(tmp(size(a)),source=a)
        deallocate(a)
        allocate(a(size(tmp)+1),mold=e)
        dummy = memcpy(loc(a(1)),loc(tmp),sizeof(tmp))
        dummy = memcpy(loc(a(size(tmp)+1)),loc(e),sizeof(e))
      end if
    end subroutine
end

