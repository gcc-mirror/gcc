! { dg-do compile }
! Test the fix for PR59198, where the field for the component 'term' in
! the derived type 'decay_gen_t' was not being built.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module decays
  abstract interface
     function obs_unary_int ()
     end function obs_unary_int
  end interface

  type, abstract :: any_config_t
   contains
     procedure (any_config_final), deferred :: final
  end type any_config_t

  type :: decay_term_t
     type(unstable_t), dimension(:), pointer :: unstable_product => null ()
  end type decay_term_t

  type, abstract :: decay_gen_t
     type(decay_term_t), dimension(:), allocatable :: term
     procedure(obs_unary_int),   nopass, pointer :: obs1_int  => null ()
  end type decay_gen_t

  type, extends (decay_gen_t) :: decay_root_t
   contains
     procedure :: final => decay_root_final
  end type decay_root_t

  type, abstract :: rng_t
  end type rng_t

  type, extends (decay_gen_t) :: decay_t
     class(rng_t), allocatable :: rng
   contains
     procedure :: final => decay_final
  end type decay_t

  type, extends (any_config_t) :: unstable_config_t
   contains
     procedure :: final => unstable_config_final
  end type unstable_config_t

  type :: unstable_t
     type(unstable_config_t), pointer :: config => null ()
     type(decay_t), dimension(:), allocatable :: decay
  end type unstable_t

  interface
     subroutine any_config_final (object)
       import
       class(any_config_t), intent(inout) :: object
     end subroutine any_config_final
  end interface

contains
  subroutine decay_root_final (object)
    class(decay_root_t), intent(inout) :: object
  end subroutine decay_root_final

  recursive subroutine decay_final (object)
    class(decay_t), intent(inout) :: object
  end subroutine decay_final

  recursive subroutine unstable_config_final (object)
    class(unstable_config_t), intent(inout) :: object
  end subroutine unstable_config_final

end module decays
