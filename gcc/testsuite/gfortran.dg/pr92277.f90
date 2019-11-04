! { dg-do compile }
!
! PR fortran/92277
!
! Contributed by José Rui Faustino de Sousa
!
module arr_m
  implicit none
contains
  subroutine arr_set(this, that)
    integer, intent(out) :: this(..)
    integer, optional, intent(out) :: that(..)

    interface
      subroutine arr_set_c(this) bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        implicit none
        integer(kind=c_int), intent(out) :: this(..)
      end subroutine arr_set_c
      subroutine arr_set_c_opt(this) bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        implicit none
        integer(kind=c_int), optional, intent(out) :: this(..)
      end subroutine arr_set_c_opt
    end interface

    call arr_set_c(this)
    call arr_set_c(that)
    call arr_set_c_opt(this)
    call arr_set_c_opt(that)
  end subroutine arr_set
end module arr_m
