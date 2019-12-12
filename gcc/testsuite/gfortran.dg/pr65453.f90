! { dg-do compile }
! PR fortran/65453
! Contributed by Tobias Burnus  <burnus at gcc.gnu.org>
procedure() :: foo   ! { dg-error "(1)" }
  contains
    subroutine foo() ! { dg-error "clashes with procedure" }
    end
end ! { dg-error "Two main PROGRAMs" }
