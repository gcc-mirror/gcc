! { dg-do compile }

! Check duplicate mapper detection in module reader.

module mod1
type S
integer, dimension(:), pointer :: arr
end type S
!$omp declare mapper (S :: v) map(to: v%arr) map(tofrom:v%arr(1))
end module mod1

module mod2
type S
character :: c
integer, dimension(:), pointer :: arr
end type S
!$omp declare mapper (S :: v) map(to: v%arr) map(tofrom:v%arr(:))

type(S) :: svar

contains

subroutine setup
allocate(svar%arr(10))
end subroutine setup

subroutine teardown
deallocate(svar%arr)
end subroutine teardown

end module mod2

program myprog
use mod1  ! { dg-error "Previous \\\!\\\$OMP DECLARE MAPPER from module mod1" }
use mod2  ! { dg-error "Ambiguous \\\!\\\$OMP DECLARE MAPPER from module mod2" }

call setup

!$omp target
svar%arr(1) = svar%arr(1) + 1
!$omp end target

call teardown

end program myprog
