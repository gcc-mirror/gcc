! { dg-do run }
! { dg-options "-std=f2003 " }
! Pointer intent test
! PR fortran/29624
!
! Valid program
program test
 implicit none
 type myT
   integer          :: x
   integer, pointer :: point
 end type myT
 integer, pointer :: p
 type(myT), pointer :: t
 type(myT) :: t2
 allocate(p,t)
 allocate(t%point)
 t%point = 55
 p = 33
 call a(p,t)
 deallocate(p)
 nullify(p)
 call a(p,t)
 t2%x     = 5
 allocate(t2%point)
 t2%point = 42
 call nonpointer(t2)
 if(t2%point /= 7) STOP 1
contains
  subroutine a(p,t)
    integer, pointer,intent(in)    :: p
    type(myT), pointer, intent(in) :: t
    integer, pointer :: tmp
    if(.not.associated(p)) return
    if(p /= 33) STOP 2
    p = 7
    if (associated(t)) then
      ! allocating is valid as we don't change the status
      ! of the pointer "t", only of it's target
      t%x = -15
      if(.not.associated(t%point)) STOP 3
      if(t%point /= 55) STOP 4
      nullify(t%point)
      allocate(tmp)
      t%point => tmp
      deallocate(t%point)
      t%point => null(t%point)
      tmp => null(tmp)
      allocate(t%point)
      t%point = 27
      if(t%point /= 27) STOP 5
      if(t%x     /= -15) STOP 6
      call foo(t)
      if(t%x     /=  32) STOP 7
      if(t%point /= -98) STOP 8
    end if
    call b(p)
    if(p /= 5) STOP 9
  end subroutine
  subroutine b(v)
    integer, intent(out) :: v
    v = 5
  end subroutine b
  subroutine foo(comp)
    type(myT), intent(inout) :: comp
    if(comp%x     /= -15) STOP 10
    if(comp%point /=  27) STOP 11
    comp%x     = 32
    comp%point = -98
  end subroutine foo
  subroutine nonpointer(t)
     type(myT), intent(in) :: t
     if(t%x     /= 5 ) STOP 12
     if(t%point /= 42) STOP 13
     t%point = 7
  end subroutine nonpointer
end program
