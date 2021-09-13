! { dg-do run }
! { dg-options "-fcheck=pointer" }
!
! Test the fix for PR99602 in which the runtime error,
! "Proc-pointer actual argument 'model' is not associated" was triggered
! by the NULL result from model%get_par_data_ptr ("tea ")
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module model_data
  type :: model_data_t
     type(modelpar_real_t), dimension(:), pointer :: par_real => null ()
   contains
     procedure :: get_par_data_ptr => model_data_get_par_data_ptr_name
     procedure :: set => field_data_set
  end type model_data_t

  type :: modelpar_real_t
     character (4) :: name
     real(4) :: value
  end type modelpar_real_t

  type(modelpar_real_t), target :: names(2) = [modelpar_real_t("foo ", 1.0), &
                                               modelpar_real_t("bar ", 2.0)]
  integer :: return_value = 0

contains

  function model_data_get_par_data_ptr_name (model, name) result (ptr)
    class(model_data_t), intent(in) :: model
    character (*), intent(in) :: name
    class(modelpar_real_t), pointer :: ptr
    integer :: i
    ptr => null ()
    do i = 1, size (model%par_real)
       if (model%par_real(i)%name == name) ptr => model%par_real(i)
    end do
  end function model_data_get_par_data_ptr_name

  subroutine field_data_set (this, ptr)
    class(model_data_t), intent(inout) :: this
    class(modelpar_real_t), intent(in), pointer :: ptr
    if (associated (ptr)) then
      return_value = int (ptr%value)
    else
      return_value = -1
    end if
  end subroutine

end module model_data

  use model_data
  class(model_data_t), allocatable :: model
  class(modelpar_real_t), pointer :: name_ptr

  allocate (model_data_t :: model)
  model%par_real => names

  call model%set (model%get_par_data_ptr ("bar "))
  if (return_value .ne. 2) stop 1
  call model%set (model%get_par_data_ptr ("tea ")) ! Triggered runtime error
  if (return_value .ne. -1) stop 2
end

