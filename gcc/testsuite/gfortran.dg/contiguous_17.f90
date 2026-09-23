! { dg-do run }
!
! PR fortran/125998
! Check the correct passing of a transposed array to an assumed-shape dummy
! having the contiguous and target attributes.
!
! Original testcase from Federico Perini <federico.perini@gmail.com>

subroutine original
    real(8) :: a(3,4)
    integer :: dims(2)           
    character(48) :: iomsg  
    call show(transpose(a),dims)
    if (any(dims/=shape(transpose(a)))) then 
        write(iomsg,1) 'invalid a^T dims',dims, &
                       ' should be',shape(transpose(a))
        error stop iomsg
    endif
    1 format(*(a,' [',i0,',',i0,']'))

contains

    subroutine show(x,dims)
        ! "3 4" with `contiguous` (BUG); "4 3" without
        real(8), intent(in), target, contiguous :: x(:,:)
        integer, intent(out) :: dims(2)    
        dims = [size(x,1),size(x,2)]
    end 

end

module m
    implicit none
    private
    public :: check_all

contains

    subroutine abort_different(lx,x,ly,y,code)
        character(*), intent(in) :: lx, ly
        integer, intent(in) :: x(..), y(..), code
        write(6,'(a," /= ",a)') lx, ly
        select rank(x)
            rank(1)
                write(6,3) lx
                write(6,*) x
            rank(2)
                write(6,3) lx
                write(6,*) x
            rank default
        end select
        select rank(y)
            rank(1)
                write(6,3) ly
                write(6,*) y
            rank(2)
                write(6,3) ly
                write(6,*) y
            rank default
        end select
        error stop code
      3 format(a,":")
    end subroutine

    subroutine check_target(x,expected,num_error)
        integer, intent(in), target, contiguous :: x(:,:)
        integer, intent(in) :: num_error, expected(:,:)
        if (any(shape(x) /= shape(expected))) then
            call abort_different( &
                    "shape(x)",shape(x), &
                    "shape(expected)",shape(expected), &
                    num_error*10+1 &
            )
        end if
        if (any(x /= expected)) then
            call abort_different("x",x,"expected",expected,num_error*10+2)
        end if
        if (x(2,2) /= expected(2,2)) then
            call abort_different( &
                "x(2,2)",x(2,2), &
                "expected(2,2)",expected(2,2), &
                num_error*10+3 &
            )
        end if
    end 

    subroutine check_non_target(x,expected,num_error)
        integer, intent(in), contiguous :: x(:,:)
        integer, intent(in) :: num_error, expected(:,:)
        if (any(shape(x) /= shape(expected))) then
            call abort_different( &
                    "shape(x)",shape(x), &
                    "shape(expected)",shape(expected), &
                    num_error*10+1 &
            )
        end if
        if (any(x /= expected)) then
            call abort_different("x",x,"expected",expected,num_error*10+2)
        end if
        if (x(2,2) /= expected(2,2)) then
            call abort_different( &
                "x(2,2)",x(2,2), &
                "expected(2,2)",expected(2,2), &
                num_error*10+3 &
            )
        end if
    end 

    subroutine wrapper_target(x,expected,num_error)
        integer, intent(in), target :: x(:,:)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: num_error
        call check_target(x,expected,num_error)
    end subroutine

    subroutine wrapper_non_target(x,expected,num_error)
        integer, intent(in) :: x(:,:), expected(:,:)
        integer, intent(in) :: num_error
        call check_non_target(x,expected,num_error)
    end subroutine

    subroutine wrapper_transpose_target(x,expected,num_error)
        integer, intent(in), target :: x(:,:)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: num_error
        call check_target(transpose(x),expected,num_error)
    end subroutine

    subroutine wrapper_transpose_non_target(x,expected,num_error)
        integer, intent(in) :: x(:,:), expected(:,:)
        integer, intent(in) :: num_error
        call check_non_target(transpose(x),expected,num_error)
    end subroutine

    subroutine check_all
        integer :: a(3,4)

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call check_target( &
                transpose(a), &
                reshape([0,2,8,34,1,3,13,55,1,5,21,89],[4,3]), &
                1 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call check_non_target( &
                transpose(a), &
                reshape([0,2,8,34,1,3,13,55,1,5,21,89],[4,3]), &
                2 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_target( &
                transpose(a), &
                reshape([0,2,8,34,1,3,13,55,1,5,21,89],[4,3]), &
                3 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_non_target( &
                transpose(a), &
                reshape([0,2,8,34,1,3,13,55,1,5,21,89],[4,3]), &
                4 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_transpose_target( &
                a, &
                reshape([0,2,8,34,1,3,13,55,1,5,21,89],[4,3]), &
                5 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_transpose_non_target( &
                a, &
                reshape([0,2,8,34,1,3,13,55,1,5,21,89],[4,3]), &
                6 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_transpose_target( &
                transpose(a), &
                reshape([0,1,1,2,3,5,8,13,21,34,55,89],[3,4]), &
                7 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_transpose_non_target( &
                transpose(a), &
                reshape([0,1,1,2,3,5,8,13,21,34,55,89],[3,4]), &
                8 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call check_target( &
                a(::2,::2), &
                reshape([0,1,8,21],[2,2]), &
                9 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call check_non_target( &
                a(::2,::2), &
                reshape([0,1,8,21],[2,2]), &
                10 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_target( &
                a(::2,::2), &
                reshape([0,1,8,21],[2,2]), &
                11 &
        )

        a = reshape([0,1,1,2,3,5,8,13,21,34,55,89],shape(a))
        call wrapper_non_target( &
                a(::2,::2), &
                reshape([0,1,8,21],[2,2]), &
                12 &
        )

    end subroutine

end module

use m
call original
call check_all
end
