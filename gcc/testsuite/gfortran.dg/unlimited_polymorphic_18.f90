! { dg-do run }
! Testing fix for
! PR fortran/60414
!
module m
    implicit none
    Type T
        real, public :: expectedScalar;
    contains
        procedure :: FCheck
        procedure :: FCheckArr
        generic :: Check => FCheck, FCheckArr
    end Type

contains

    subroutine FCheck(this,X)
        class(T) this
        class(*) X
        real :: r
        select type (X)
            type is (real)
                if ( abs (X - this%expectedScalar) > 0.0001 ) then
                    STOP 1
                end if
            class default
                STOP 2
         end select
    end subroutine FCheck

    subroutine FCheckArr(this,X)
        class(T) this
        class(*) X(:)
        integer i
        do i = 1,6
            this%expectedScalar = i - 1.0
            call this%FCheck(X(i))
        end do
    end subroutine FCheckArr

    subroutine CheckTextVector(vec, n, scal)
        integer, intent(in) :: n
        class(*), intent(in) :: vec(n)
        class(*), intent(in) :: scal
        integer j
        Type(T) :: Tester

        ! Check full vector
        call Tester%Check(vec)
        ! Check a scalar of the same class like the vector
        Tester%expectedScalar = 5.0
        call Tester%Check(scal)
        ! Check an element of the vector, which is a scalar
        j=3
        Tester%expectedScalar = 2.0
        call Tester%Check(vec(j))

    end subroutine CheckTextVector

end module

program test
   use :: m
   implicit none

   real :: vec(1:6) = (/ 0, 1, 2, 3, 4, 5 /)
   call checktextvector(vec, 6, 5.0)
end program test

