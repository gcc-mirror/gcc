! { dg-do compile }

module data_func_mod
    implicit none
    integer, parameter :: sp = 4
    type :: data_type
        real(kind=sp), pointer, dimension(:, :) :: data => null()
        integer :: nr_rows = 0, nr_cols = 0
    end type data_type

contains

    function get_row(this, i) result(row)
        implicit none
        type(data_type), intent(in) :: this
        integer, intent(in) :: i
        real(kind=sp), dimension(this%nr_cols) :: row
        row = this%data(:, i)
    end function get_row

    subroutine print_matrix(m, i, fmt_str)
        implicit none
        class(data_type), intent(in) :: m
        integer, intent(in) :: i
        character(len=20), intent(in) :: fmt_str
        write (unit=6, fmt=fmt_str) get_row(m, i)
    end subroutine print_matrix

end module data_func_mod
