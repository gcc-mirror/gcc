! { dg-do compile }
!
! PR 50547: dummy procedure argument of PURE shall be PURE
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

elemental function fun (sub)
  interface
    pure subroutine sub  ! { dg-error "not allowed in elemental procedure" }
    end subroutine
  end interface
end function
