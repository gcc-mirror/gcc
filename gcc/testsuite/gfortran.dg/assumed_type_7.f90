! { dg-do compile }
!
! PR 54190: TYPE(*)/assumed-rank: Type/rank check too relaxed for dummy procedure
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

implicit none
call sub(f)    ! { dg-error "Type mismatch in argument" }
contains

  subroutine f(x)
    type(*) :: x
  end subroutine

  subroutine sub(g)
    interface
      subroutine g(x)
        integer :: x
      end subroutine
    end interface
  end subroutine

end 
