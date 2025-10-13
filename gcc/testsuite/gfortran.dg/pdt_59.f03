! { dg-do compile }
!
! Test the fix for PR122191, which used to ICE in compilation.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module input_output_pair_m
  implicit none

  type input_output_pair_t(k)
    integer, kind :: k
    integer :: a, b
  end type

  type mini_batch_t(k)
    integer, kind :: k = kind(1.)
    type(input_output_pair_t(k)), allocatable :: input_output_pairs_(:)
  end type

  interface

    module function default_real_construct()
      implicit none
      type(mini_batch_t) default_real_construct
    end function

  end interface

end module

submodule(input_output_pair_m) input_output_pair_smod
contains
  function default_real_construct()
   type(mini_batch_t) default_real_construct
   allocate (default_real_construct%input_output_pairs_(2))
   default_real_construct%input_output_pairs_%a = [42,43]
   default_real_construct%input_output_pairs_%b = [420,421]
  end
end submodule

  use input_output_pair_m
  type(mini_batch_t), allocatable :: res
  res = default_real_construct()
  if (any (res%input_output_pairs_%a /= [42,43])) stop 1
  if (any (res%input_output_pairs_%b /= [420,421])) stop 2
  if (allocated (res)) deallocate (res)
end
