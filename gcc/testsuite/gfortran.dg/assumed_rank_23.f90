! { dg-do compile }
!
! PR fortran/54753
! TS29113:C535c
! F2018:C839
!
module m

  interface
    subroutine s1 (x, y)
      class(*) :: x(..)
      class(*), intent (out) :: y(..)
    end subroutine
  end interface

end module 
