! { dg-do compile }
! PR fortran/107707 - ICE in gfc_compare_actual_formal
! Contributed by G.Steinmetz

program p
  character(3), allocatable :: c
  c = 'abc'
  call s(c)
contains
  subroutine s(x)
    character(real(3)), allocatable :: x ! { dg-error "must be of INTEGER type" }
  end
end
