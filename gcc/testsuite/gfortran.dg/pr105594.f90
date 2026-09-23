! { dg-do compile }
! { dg-options "-Wuse-without-only" }
! PR105594: Spurious warning when compiled with -Wuse-without-only
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
!
module foo_m
    interface
        module subroutine foo
        end subroutine
    end interface
end module

submodule (foo_m) foo_s
contains
    module procedure foo
    end procedure
end submodule
