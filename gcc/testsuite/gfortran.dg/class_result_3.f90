! { dg-do compile }
!
! PR 78300: [OOP] Failure to compile a F03 code with an optional dummy procedure argument
!
! Contributed by DIL <liakhdi@ornl.gov>

  implicit none

  type gfc_cont_elem_t
  end type

  contains

    function gfc_copy_i() result(clone)
      class(gfc_cont_elem_t), pointer:: clone
    end

    subroutine ContElemConstruct(copy_constr_func)
      procedure(gfc_copy_i) :: copy_constr_func
    end

end
