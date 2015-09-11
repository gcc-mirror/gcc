! { dg-do run }
!
! PR fortran/65792
! The evaluation of the argument in the call to new_prt_spec2
! failed to properly initialize the comp component.
! While the array contents were properly copied, the array bounds remained
! uninitialized.
!
! Contributed by Dominique D'Humieres <dominiq@lps.ens.fr>

program main
  implicit none

  integer, parameter :: n = 2

  type :: string_t
     character(LEN=1), dimension(:), allocatable :: chars
  end type string_t

  type :: string_container_t
     type(string_t) :: comp
  end type string_container_t

  type(string_t) :: prt_in, tmp, tmpa(n)
  type(string_container_t) :: tmpc, tmpca(n)
  integer :: i, j, k

  do i=1,2

! scalar elemental function with structure constructor
     prt_in = string_t(["D"])
     tmpc = new_prt_spec2 (string_container_t(prt_in))
     if (any(tmpc%comp%chars .ne. ["D"])) call abort
     deallocate (prt_in%chars)
     deallocate(tmpc%comp%chars)
! Check that function arguments are OK too
     tmpc = new_prt_spec2 (string_container_t(new_str_t(["h","e","l","l","o"])))
     if (any(tmpc%comp%chars .ne. ["h","e","l","l","o"])) call abort
     deallocate(tmpc%comp%chars)

  end do

contains

  impure elemental function new_prt_spec2 (name) result (prt_spec)
    type(string_container_t), intent(in) :: name
    type(string_container_t) :: prt_spec
    prt_spec = name
  end function new_prt_spec2


  function new_str_t (name) result (prt_spec)
    character (*), intent(in), dimension (:) :: name
    type(string_t) :: prt_spec
    prt_spec = string_t(name)
  end function new_str_t

end program main

