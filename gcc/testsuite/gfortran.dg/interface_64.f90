! { dg-do compile }
!
! Patch 9/16 of the series associated with PR82173 intreoduced a
! regression that allowed the invalid code below to compile.
!
! Contributed by Mikael Morin  <morin-mikael@orange.fr>
!
program prog
    implicit none
    interface
        integer function foo(arg)
                integer, intent(in) :: arg
       end function
        integer function bar(arg1, arg2)
                integer, intent(in) :: arg1, arg2
       end function
    end interface
    procedure(foo), pointer :: p
    procedure(bar), pointer :: q
    p => null(q) ! { dg-error "Interface mismatch in procedure pointer assignment" }
end program
