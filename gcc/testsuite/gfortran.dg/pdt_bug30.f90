! { dg-do run }
!
! PR128876
!
! Local variable of a PDT type with a component of a recursive PDT type:
! the default initialization is an assignment from a constructor
! temporary, which first deallocates the old value of l%node%next, i.e.
! uninitialized stack.  free(): invalid pointer / SIGSEGV at run time.
! Regression on trunk; works with 16.2.1.  Found running the fiats tests
! (__deallocate_neuron_m_PDTneuron_t_4 in neural_network from_json).
!
! Contributed by Jerry Delisle  <delisle@gcc.gnu.org>
!
module pdt_rec_comp_m
  implicit none
  type node_t(k)
    integer, kind :: k = kind(1.0)
    real(k) :: b
    type(node_t(k)), allocatable :: next
  end type
  type layer_t(k)
    integer, kind :: k = kind(1.0)
    type(node_t(k)) :: node
  end type
contains
  subroutine s (r)
    real, intent(out) :: r
    type(layer_t) :: l
    l%node%b = 1.0
    allocate (l%node%next)
    l%node%next%b = 2.0
    r = l%node%next%b
  end subroutine
end module
program p
  use pdt_rec_comp_m
  implicit none
  real :: r
  call s (r)
  if (r /= 2.0) stop 1
end program
