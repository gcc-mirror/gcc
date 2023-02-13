! { dg-do compile }
! PR fortran/108421
! Contributed by G.Steinmetz

program p
  character(real(3)) :: c ! { dg-error "must be of INTEGER type" }
  call s(c)
end
subroutine s(x)
  character(*) :: x
end
