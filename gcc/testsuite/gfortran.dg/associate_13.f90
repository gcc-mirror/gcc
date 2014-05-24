! { dg-do run }
!
! Tests the fix for PR56047.  This is actually a development of
! the test case of comment #10.
!
! Reported by Juergen Reuter  <juergen.reuter@desy.de>
!
  implicit none
  type :: process_variant_def_t
    integer :: i
  end type
  type :: process_component_def_t
     class(process_variant_def_t), allocatable :: variant_def
  end type
  type(process_component_def_t), dimension(1:2) :: initial
  allocate (initial(1)%variant_def, source = process_variant_def_t (99))
  associate (template => initial(1)%variant_def)
    template%i = 77
  end associate
  if (initial(1)%variant_def%i .ne. 77) call abort
end
