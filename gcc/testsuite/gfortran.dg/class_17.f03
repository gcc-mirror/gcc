! { dg-do compile }
!
! PR 43696: [OOP] Bogus error: Passed-object dummy argument must not be POINTER
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>


MODULE error_stack_module
  implicit none

  type,abstract::serializable_class
   contains
     procedure(ser_DTV_RF),deferred::read_formatted
  end type serializable_class

  abstract interface
     subroutine ser_DTV_RF(dtv,unit,iotype,v_list,iostat,iomsg)
       import serializable_class
       CLASS(serializable_class),INTENT(INOUT) :: dtv
       INTEGER, INTENT(IN) :: unit
       CHARACTER (LEN=*), INTENT(IN) :: iotype
       INTEGER, INTENT(IN) :: v_list(:)
       INTEGER, INTENT(OUT) :: iostat
       CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
     end subroutine ser_DTV_RF
  end interface

  type,extends(serializable_class)::error_type
     class(error_type),pointer::next=>null()
   contains
     procedure::read_formatted=>error_read_formatted
  end type error_type

contains

  recursive subroutine error_read_formatted(dtv,unit,iotype,v_list,iostat,iomsg)
    CLASS(error_type),INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: v_list(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    character(8),allocatable::type
    character(8),allocatable::next
    call basic_read_string(unit,type)
    call basic_read_string(unit,next)
    if(next=="NEXT")then
       allocate(dtv%next)
       call dtv%next%read_formatted(unit,iotype,v_list,iostat,iomsg)
    end if
  end subroutine error_read_formatted

end MODULE error_stack_module


module b_module
  implicit none
  type::b_type
     class(not_yet_defined_type_type),pointer::b_component  ! { dg-error "is a type that has not been declared" }
  end type b_type
end module b_module
 
