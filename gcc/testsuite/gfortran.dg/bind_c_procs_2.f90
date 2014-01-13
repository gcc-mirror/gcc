! { dg-do compile }
!
! PR 59023: [4.9 regression] ICE in gfc_search_interface with BIND(C)
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>

  type t
    integer hidden
  end type
  
contains

  subroutine bar
    type(t) :: toto
    interface
      integer function helper() bind(c)
      end function
    end interface
    toto = t(helper())
  end subroutine

end
