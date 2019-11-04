! { dg-do run }
! { dg-additional-sources bind_c_array_params_3_aux.c }
!
! PR fortran/92284
!
! Contributed by José Rui Faustino de Sousa
!
program arr_p
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none (type, external)

  integer(kind=c_int), pointer :: arr(:)
  integer :: i

  nullify(arr)
  call arr_set(arr)

  if (.not.associated(arr)) stop 1
  if (lbound(arr,dim=1) /= 1) stop 2
  if (ubound(arr,dim=1) /= 9) stop 3
  if (any (arr /= [(i, i=0,8)])) stop 4
  deallocate(arr)

contains

  subroutine arr_set(this) !bind(c)
    integer(kind=c_int), pointer, intent(out) :: this(:)

    interface
      subroutine arr_set_c(this) bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        implicit none
        integer(kind=c_int), pointer, intent(out) :: this(:)
      end subroutine arr_set_c
    end interface

    call arr_set_c(this)
  end subroutine arr_set
end program arr_p
