! { dg-do compile }
! This checks the fix for PR 26064
!
! Contributed by Sven Buijssen <sven.buijssen@math.uni-dortmund.de>
module ice
  implicit none
  contains

    subroutine foo()
    contains

      subroutine bar(baz)
        integer, optional :: baz
        if (present(baz)) then
        endif
      end subroutine bar
    end subroutine foo
end module
