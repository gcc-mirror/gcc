! { dg-do compile }
!
! PR 59941: [4.7 Regression] [OOP] ICE with polymorphic types
!
! Contributed by Jürgen Reuter <juergen.reuter@desy.de>

module tao_random_numbers
  integer, dimension(10), private :: s_buffer
  integer, private :: s_buffer_end = size (s_buffer)
end module


module beam_structures

  private

  type :: beam_structure_t
     integer, dimension(:), allocatable :: smatrix
   contains
     procedure :: get_smatrix
  end type
  
contains

  function get_smatrix (beam_structure) result (matrix)
    class(beam_structure_t), intent(in) :: beam_structure
    integer, dimension (size (beam_structure%smatrix)) :: matrix
  end function
  
end module


program p
  use tao_random_numbers
  use beam_structures
end

! { dg-final { cleanup-modules "tao_random_numbers beam_structures" } }
