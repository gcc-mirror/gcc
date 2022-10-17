! { dg-do compile }
! PR fortran/86551 - ICE on invalid code with select type / end select type

subroutine b
  type :: t1
  end type t1
  class(t1) :: c2
  select type (d => c2)
  end select type       ! { dg-error "Syntax error" }
end                     ! { dg-error "END SELECT statement expected" }

! { dg-prune-output "Unexpected end of file" }
