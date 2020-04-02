! { dg-compile }
!
! Reported by Vladimir Nikishkin
! at https://stackoverflow.com/questions/60972134/whats-wrong-with-the-following-fortran-code-gfortran-dtio-dummy-argument-at#
!

module scheme

  type, abstract :: scheme_object
   contains
     procedure, pass :: generic_scheme_print => print_scheme_object
     generic, public :: write (formatted) => generic_scheme_print
  end type scheme_object

  abstract interface
     subroutine packageable_procedure(  )
       import scheme_object
     end subroutine packageable_procedure
  end interface
contains

  subroutine print_scheme_object(this, unit, iotype, v_list, iostat, iomsg)
    class(scheme_object), intent(in) :: this
    integer, intent(in)         :: unit
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: v_list (:)
    integer, intent(out)        :: iostat
    character(*), intent(inout) :: iomsg
    iostat = 1
  end subroutine print_scheme_object

  subroutine packaged_cons( )
  end subroutine packaged_cons

  function make_primitive_procedure_object( proc1 ) result( retval )
    class(scheme_object), pointer :: retval
    procedure(packageable_procedure), pointer :: proc1
  end function make_primitive_procedure_object

  subroutine ll_setup_global_environment()
    procedure(packageable_procedure), pointer :: proc1
    class(scheme_object), pointer :: proc_obj_to_pack
    proc1 => packaged_cons
    proc_obj_to_pack => make_primitive_procedure_object( proc1 )
  end subroutine ll_setup_global_environment

end module scheme

program main
end program main
