! { dg-do compile }
!
! PR 40451: [F03] procedure pointer assignment rejected 
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

contains

  function f()
    intrinsic :: sin
    procedure(sin), pointer :: f
    f => sin
  end function f

end

