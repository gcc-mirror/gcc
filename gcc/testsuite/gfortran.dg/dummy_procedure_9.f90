! { dg-do compile }
!
! PR 40453: [F95] Enhanced (recursive) argument checking
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program RecursiveInterface
  
  call c(b2)  ! { dg-error "Interface mismatch in dummy procedure" }

 contains

    subroutine a1(x)
      real :: x
    end subroutine

    subroutine a2(i)
      integer :: i
    end subroutine

    !!!!!!!!!!!!!!!

    subroutine b1 (f1)
      procedure(a1) :: f1
    end subroutine

    subroutine b2 (f2)
      procedure(a2) :: f2
    end subroutine

    !!!!!!!!!!!!!!!

    subroutine c(g)
     procedure(b1) :: g
    end subroutine
  
end
