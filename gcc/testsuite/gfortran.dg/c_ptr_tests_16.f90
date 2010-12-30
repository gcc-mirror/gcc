! { dg-do compile }
! { dg-options "-fdump-tree-optimized -O" }
!
! PR fortran/46974

program test
  use ISO_C_BINDING
  implicit none
  type(c_ptr) :: m
  integer(c_intptr_t) :: a
  integer(transfer(transfer(4_c_intptr_t, c_null_ptr),1_c_intptr_t)) :: b
  a = transfer (transfer("ABCE", m), 1_c_intptr_t)
  print '(z8)', a
  if (     int(z'45434241') /= a  &
     .and. int(z'41424345') /= a  &
     .and. int(z'4142434500000000',kind=8) /= a) &
    call i_do_not_exist()
end program test

! Examples contributed by Steve Kargl and James Van Buskirk

subroutine bug1
   use ISO_C_BINDING
   implicit none
   type(c_ptr) :: m
   type mytype
     integer a, b, c
   end type mytype
   type(mytype) x
   print *, transfer(32512, x)  ! Works.
   print *, transfer(32512, m)  ! Caused ICE.
end subroutine bug1 

subroutine bug6
   use ISO_C_BINDING
   implicit none
   interface
      function fun()
         use ISO_C_BINDING
         implicit none
         type(C_FUNPTR) fun
      end function fun
   end interface
   type(C_PTR) array(2)
   type(C_FUNPTR) result
   integer(C_INTPTR_T), parameter :: const(*) = [32512,32520]

   result = fun()
   array = transfer([integer(C_INTPTR_T)::32512,32520],array)
!   write(*,*) transfer(result,const)
!   write(*,*) transfer(array,const)
end subroutine bug6

function fun()
   use ISO_C_BINDING
   implicit none
   type(C_FUNPTR) fun
   fun = transfer(32512_C_INTPTR_T,fun)
end function fun 

! { dg-final { scan-tree-dump-times "i_do_not_exist" 0 "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
