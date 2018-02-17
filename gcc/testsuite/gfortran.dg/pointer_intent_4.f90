! { dg-do run }
! { dg-options "-fno-inline" }
!
! PR fortran/46937
!
! Check that a non-pointer INTENT(IN) dummy
! with pointer component is properly treated
!
program test
 type myT
   integer, pointer :: point
 end type myT
 type(myT) :: t2
 allocate(t2%point)
 t2%point = 42
 call nonpointer(t2)
 if(t2%point /= 7) STOP 1
 t2%point = 42
 call nonpointer2(t2)
 if(t2%point /= 66) STOP 2
contains
  subroutine nonpointer(t)
     type(myT), intent(in) :: t
     t%point = 7
  end subroutine nonpointer
  subroutine nonpointer2(t)
     class(myT), intent(in) :: t
     t%point = 66
  end subroutine nonpointer2
end program
