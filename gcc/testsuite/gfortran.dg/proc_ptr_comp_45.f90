! { dg-do run }
! Test the fix for PR59198, where the field for the component 'term' in
! the derived type 'decay_gen_t' was not being built.
!
! Contributed by Paul Thomas and based on the original testcase by
! Juergen Reuter  <juergen.reuter@desy.de>
!
module decays

  implicit none

  interface
    real elemental function iface (arg)
      real, intent(in) :: arg
    end function
  end interface

  type :: decay_term_t
     type(decay_t), pointer :: unstable_product
     integer :: i
  end type

  type :: decay_gen_t
     procedure(iface), nopass, pointer :: obs1_int
     type(decay_term_t), allocatable :: term
  end type

  type :: rng_t
    integer :: i
  end type

  type, extends (decay_gen_t) :: decay_t
     class(rng_t), allocatable :: rng
  end type

  class(decay_t), allocatable :: object

end

  use decays
  type(decay_t), pointer :: template
  real, parameter :: arg = 1.570796327
  allocate (template)
  allocate (template%rng)
  template%obs1_int => cos
  if (abs (template%obs1_int (arg) - cos (arg)) .gt. 1e-4) call abort
  allocate (object, source = template)
  if (abs (object%obs1_int (arg) - cos (arg)) .gt. 1e-4) call abort
end
