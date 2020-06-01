! { dg-do compile }

program test
   implicit none

   type :: t1
     integer :: i
   end type

   type, extends(t1) :: t2
   end type

   class(t1), allocatable :: array1(:,:)
   class(t2), allocatable :: array2(:,:)

   allocate(array1(3,3))
   allocate(array2(3,3))

   select type(b => foo(1))
      type is (t1)
         b%i = 1
      type is (t2)
         call sub_with_in_and_inout_param(b,b)
   end select

   contains

     function foo(i)
       integer :: U(2)
       integer :: i
       class(t1), POINTER :: foo(:)
       ALLOCATE(foo(2))
       U = [ 1,2 ]
       if (i>0) then
         foo => array1(2,U)
       else
         foo => array2(2,U)
       end if
     end function

     subroutine sub_with_in_and_inout_param(y, z)
        type(t2), INTENT(IN) :: y(:)
        class(t2), INTENT(INOUT) :: z(:)
        z%i = 10
     end subroutine

end

! { dg-error "cannot be used in a variable definition context .assignment."  " " { target *-*-* } 21 }
! { dg-error "cannot be used in a variable definition context .actual argument to INTENT = OUT.INOUT."  " " { target *-*-* } 23 }
! { dg-error "Pointer assignment target is neither TARGET nor POINTER" " " { target *-*-* } 35 }
! { dg-error "Pointer assignment target is neither TARGET nor POINTER" " " { target *-*-* } 37 }

