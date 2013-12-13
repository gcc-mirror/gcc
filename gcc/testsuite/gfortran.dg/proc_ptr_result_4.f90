! { dg-do compile }
!
! PR 40451: [F03] procedure pointer assignment rejected 
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

contains

  function f()
    intrinsic :: sin
    abstract interface
      pure real function sin_interf(x)
        real, intent(in) :: x
      end function sin_interf
    end interface
    ! We cannot use "sin" directly as it is ELEMENTAL
    procedure(sin_interf), pointer :: f
    f => sin
  end function f

end

