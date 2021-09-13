! { dg-do run }
! { dg-additional-sources ISO_Fortran_binding_18.c }

module fortran_binding_test_18
    use iso_c_binding
    implicit none
contains

    subroutine test(array)
        integer(c_int) :: array(:)
        array = 1
    end subroutine

    function do_loop(array) result(the_sum) bind(c)
        integer(c_int), intent(in out) :: array(:,:,:)
        integer(c_int) :: the_sum, i, j

        the_sum = 0  
        array = 0
        do i=1,size(array,3)
            do j=1,size(array,2)
                call test(array(:,j,i))
            end do
        end do
        the_sum = sum(array)
    end function

end module
