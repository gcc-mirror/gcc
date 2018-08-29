! { dg-do compile }
! { dg-options "-Wunused-function" }
!
! PR 54224: [4.8 Regression] Bogus -Wunused-function warning with static function
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module mod_say_hello
    private :: hello_integer
contains
    subroutine say_hello()
        call hello_integer(123)
    end subroutine

    subroutine hello_integer( a )
        integer, intent(in) ::  a
        print *, "Hello ", a, "!"
    end subroutine
end module
