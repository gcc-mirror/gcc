! { dg-do run }
! { dg-additional-options {-fdump-tree-original} }
!
! PR fortran/128103
! Check the type and element length of the array descriptor for the polymorphic
! pointer X(:) used as TARGET of the ASSOCIATED intrinsic.  We used to generate
! a descriptor with class container element type.
!
! There are two array descriptor variables of type T1 used as actual argument,
! one for the call to SUB, and one for the TARGET of the call to ASSOCIATED:
! { dg-final { scan-tree-dump-times {struct array01_t1 parm\.\d+;} 2 {original} } }
! Each of them initialized with an element length of 4:
! { dg-final { scan-tree-dump-times {parm\.\d+\.dtype = .\.elem_len=4} 2 {original} } }

program prog
   implicit none
   integer, parameter :: n = 5
   type :: t1
      integer :: c1
   end type
   type(t1), target :: a(n)
   integer :: i
   a = [ (t1(i*i+2), i=1,n) ]
   call sub(a)
contains
   subroutine sub(x)
      class(t1), pointer, intent(in) :: x(:)
      type(t1), pointer :: p(:)
      p => a
      if (.not. associated(p, x(:))) error stop 1
   end subroutine
end program
