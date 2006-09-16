! { dg-do compile }
! PR fortran/27457
! This lead to a segfault previously.
        implicit none
        integer(kind=1) :: i
        real :: r(3)
        select case (i)
        case (129) r(4) = 0 { dg-error "Expected the name" }
        end select
        end
