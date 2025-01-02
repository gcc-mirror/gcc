! { dg-do compile }
! { dg-options "-fcheck=recursion" }
!
! Test the fix for pr115348.
!
! Contributed by Maxime van den Bossche  <maxime.vandenbossche@kuleuven.be>
!
module mymodule
    implicit none

    type mytype
        integer :: mynumber
        contains
        procedure :: myroutine
    end type mytype

    contains

    subroutine myroutine(self)
        class(mytype), intent(out) :: self

        self%mynumber = 1
    end subroutine myroutine
end module mymodule


program myprogram
    use mymodule, only: mytype
    implicit none

    type(mytype) :: myobject

    call myobject%myroutine()
    print *, myobject%mynumber
end program myprogram
