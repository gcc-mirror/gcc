! { dg-do compile }
! { dg-options "-fdefault-integer-8" }

module mymod 
  type :: my_type
    real :: value 
  end type 

  interface write(formatted) ! { dg-error "is not supported" }
    module procedure  write_formatted ! { dg-error "a generic module interface" }
  end interface ! { dg-error "Expecting" }

contains
  subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(my_type), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted
end module 
