! { dg-do compile }
!
! PR 58470: [4.9 Regression] [OOP] ICE on invalid with FINAL procedure and type extension
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module cf
  type  :: cfml
   contains
     final :: mld
  end type cfml
  type, extends(cfml) :: cfmde
  end type cfmde
contains
  subroutine mld(s)   ! { dg-error "must be of type" }
    class(cfml), intent(inout) :: s
  end subroutine mld
end module cf
