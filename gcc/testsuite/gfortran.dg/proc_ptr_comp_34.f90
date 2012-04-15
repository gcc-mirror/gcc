! { dg-do run }
!
! PR 51082: [F03] Wrong result for a pointer to a proc-pointer component
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program ala
  implicit none

  type process_list
    procedure(ala1), pointer, nopass :: process
  end type

  type(process_list), target  :: p_list
  type(process_list), pointer :: p

  p_list%process => ala1
  p => p_list

   write(*,*) p_list%process(1.0)
   write(*,*) p%process(1.0)       !!!! failed

contains

  real function ala1(x)
    real, intent(in) :: x
    ala1 = x
  end function

end program
