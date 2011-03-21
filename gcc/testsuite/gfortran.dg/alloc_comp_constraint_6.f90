! { dg-do compile }
! PR45889 Regression with I/O of element of allocatable array in derived type 
module cell
  implicit none
  private 
  type, public:: unit_cell
     integer                                             ::num_species
     character(len=8), dimension(:),          allocatable::species_symbol
  end type unit_cell
  type(unit_cell),                            public, save::current_cell 
  contains
  subroutine cell_output
    implicit none
    integer::i
    do i=1,current_cell%num_species
       write(*,*)(current_cell%species_symbol(i))
    end do
    return
  end subroutine cell_output
end module cell
! { dg-final { cleanup-modules "cell" } }
