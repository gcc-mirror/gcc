! { dg-do compile }
! { dg-prune-output "Obsolescent feature: Alternate-return argument" }
! PR fortran/103717 - ICE in doloop_code
! Contributed by G.Steinmetz

program p
  integer :: i
  do i = 1, 2
     call s(i) ! { dg-error "Missing alternate return specifier" }
  end do
contains
  subroutine s(*)
  end
end

recursive subroutine s(*)
  integer :: i
  do i = 1, 2
     call s(i) ! { dg-error "Missing alternate return specifier" }
  end do
end
