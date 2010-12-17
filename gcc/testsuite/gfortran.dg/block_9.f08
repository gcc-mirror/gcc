! { dg-do compile }
!
! PR 46849: [OOP] MODULE PROCEDURE resolution does not work in BLOCK or SELECT TYPE
!
! Contributed by Reinhold Bader <bader@lrz.de>

  implicit none

  block
    call init(fun)
  end block

contains

  subroutine init(func)
    real, external :: func
  end subroutine

  real function fun()
    fun = 1.1
  end function

end
