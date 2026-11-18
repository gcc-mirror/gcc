! { dg-do run }
!
! PR 128833
module m
  implicit none
  integer :: n_assign = 0, n_final = 0
  type :: c
     integer :: i = 0
   contains
     final     :: c_final
     procedure :: c_assign
     generic   :: assignment(=) => c_assign
  end type c
  type :: t
     type(c)          :: comp
     integer          :: j = 0
     integer, pointer :: p => null()
  end type t
contains
  subroutine c_final (self)
    type(c), intent(inout) :: self
    n_final = n_final + 1
  end subroutine c_final
  subroutine c_assign (to, from)
    class(c), intent(out) :: to
    class(c), intent(in)  :: from
    n_assign = n_assign + 1
    to%i = from%i
  end subroutine c_assign
end module m

program p
  use m
  implicit none
  type(t), pointer :: q
  type(t), target  :: r
  integer :: n_assign_ref, n_final_ref

  ! Reference: the same assignment to a variable that is not a pointer.
  r = t (c (1), 2)
  if (r%comp%i /= 1 .or. r%j /= 2) stop 1
  n_assign_ref = n_assign
  n_final_ref = n_final

  n_assign = 0
  n_final = 0
  allocate (q)
  q = t (c (1), 2)
  if (q%comp%i /= 1 .or. q%j /= 2) stop 2
  if (n_assign /= n_assign_ref .or. n_final /= n_final_ref) stop 3
  deallocate (q)
end program p
