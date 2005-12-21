! { dg-do compile }
! PR 25423: Nested WHERE constructs.
program nested_where

    implicit none
    integer :: a(4)
    logical :: mask1(4) = (/.TRUE., .TRUE., .FALSE., .FALSE./), &
               mask2(4) = (/.TRUE., .FALSE., .TRUE., .FALSE./)

    where (mask1)
        where (mask2)
            a = 1
        elsewhere
            a = 2
        end where
    elsewhere
        where (mask2)
            a = 3
        elsewhere
            a = 4
        end where
    end where

    print *, a

end program nested_where
