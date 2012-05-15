! { dg-do compile }
!
! Tests the fix for PR36454, where the PUBLIC declaration for
! aint and bint was rejected because the access was already set.
!
! Contributed by Thomas Orgis <thomas.orgis@awi.de>

module base
        integer :: baseint
end module

module a
        use base, ONLY: aint => baseint
end module

module b
        use base, ONLY: bint => baseint
end module

module c
        use a
        use b
        private
        public :: aint, bint
end module

program user
        use c, ONLY: aint, bint

        aint = 3
        bint = 8
        write(*,*) aint
end program
