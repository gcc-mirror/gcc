! { dg-do compile }

program bad_len

  implicit none

contains

  subroutine sub
    character(len = ICE) :: line ! { dg-error "INTEGER expression expected" }
  end subroutine

end program
