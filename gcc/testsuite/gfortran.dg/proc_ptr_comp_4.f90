! { dg-do compile }
!
! PR39630: Fortran 2003: Procedure pointer components.
!
! Original code by Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!
! Adapted by Janus Weil <janus@gcc.gnu.org>


! Test for infinte recursion in trans-types.c when a PPC interface
! refers to the original type.

module expressions

  type :: eval_node_t
     logical, pointer :: lval => null ()
     type(eval_node_t), pointer :: arg1 => null ()
     procedure(unary_log), nopass, pointer :: op1_log  => null ()
  end type eval_node_t

  abstract interface
     logical function unary_log (arg)
       import eval_node_t
       type(eval_node_t), intent(in) :: arg
     end function unary_log
  end interface

contains

  subroutine eval_node_set_op1_log (en, op)
    type(eval_node_t), intent(inout) :: en
    procedure(unary_log) :: op
    en%op1_log => op
  end subroutine eval_node_set_op1_log

  subroutine eval_node_evaluate (en)
    type(eval_node_t), intent(inout) :: en
    en%lval = en%op1_log  (en%arg1)
  end subroutine

end module


! Test for C_F_PROCPOINTER and pointers to derived types

module process_libraries

  implicit none

  type :: process_library_t
     procedure(), nopass, pointer :: write_list
  end type process_library_t

contains

  subroutine process_library_load (prc_lib)
    use iso_c_binding 
    type(process_library_t) :: prc_lib
    type(c_funptr) :: c_fptr
    call c_f_procpointer (c_fptr, prc_lib%write_list)
  end subroutine process_library_load

  subroutine process_libraries_test ()
    type(process_library_t), pointer :: prc_lib
    call prc_lib%write_list ()
  end subroutine process_libraries_test

end module process_libraries


! Test for argument resolution

module hard_interactions

  implicit none

  type :: hard_interaction_t
     procedure(), nopass, pointer :: new_event
  end type hard_interaction_t

  interface afv
     module procedure afv_1
  end interface

contains

  function afv_1 () result (a)
    real, dimension(0:3) :: a
  end function

  subroutine hard_interaction_evaluate (hi)
    type(hard_interaction_t) :: hi
    call hi%new_event (afv ())
  end subroutine

end module hard_interactions


! Test for derived types with PPC working properly as function result.

  implicit none

  type :: var_entry_t
    procedure(), nopass, pointer :: obs1_int
  end type var_entry_t
  
  type(var_entry_t), pointer :: var

  var => var_list_get_var_ptr ()

contains

  function var_list_get_var_ptr ()
    type(var_entry_t), pointer :: var_list_get_var_ptr
  end function var_list_get_var_ptr

end
