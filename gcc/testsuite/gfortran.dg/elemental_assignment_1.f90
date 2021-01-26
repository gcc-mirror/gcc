! { dg-do run }
! PR 67539 - this used to give a segfault at runtime.
! Test case by "mrestelli".

module m
 implicit none

 type :: t_a
  real, allocatable :: x
 end type t_a

 interface assignment(=)
  module procedure copy_t_a
 end interface

contains
 
 elemental subroutine copy_t_a(y,x)
  type(t_a), intent(in)  :: x
  type(t_a), intent(out) :: y
   allocate( y%x , source=x%x )
 end subroutine copy_t_a

 elemental function new_t_a(x) result(res)
  real, intent(in) :: x
  type(t_a) :: res
   allocate( res%x )
   res%x = x
 end function new_t_a

end module m


program p
 use m
 implicit none

 integer :: i
 type(t_a) :: tmp
 type(t_a), allocatable :: v(:)

 allocate( v(2) )

 v = new_t_a(1.5)   ! -> segmentation fault

 !tmp = new_t_a(1.5) ! -> OK
 !v = tmp

 !do i=1,size(v)     ! -> also OK
 !  v(i) = new_t_a(1.5)
 !enddo

 do i=1,size(v)
   write(*,*) " i = ",i
   write(*,*) allocated(v(i)%x)
   write(*,*) v(i)%x
 enddo
 
end program p
