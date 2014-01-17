! PR fortran/59440
! { dg-do compile }
! { dg-options "-O2 -g" }

module pr59440
  implicit none
  type t
     integer :: grid = 0
  end type t
contains
  subroutine read_nml (nnml, s)
    integer, intent(in)  :: nnml
    type(t), intent(out) :: s
    integer              :: grid
    namelist /N/ grid
    call read_nml_type_2
    s%grid = grid
  contains
    subroutine read_nml_type_2
      read (nnml, nml=N)
    end subroutine read_nml_type_2
  end subroutine read_nml
end module pr59440
