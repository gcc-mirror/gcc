! { dg-do compile }
! PR fortran/108544 - ICE in check_host_association
! Contributed by G.Steinmetz

module m
contains
  subroutine s
    select type (s => 1) ! { dg-error "Selector shall be polymorphic" }
    end select
  end
end
