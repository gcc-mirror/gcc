! { dg-do run }
! Used to ICE at line 10 in simplify.cc(gfc_simplify_len).
! See attachment to PR82173 comment 10 - reproducers/bug1-simplify_len_ice.f90
program r6
  type :: t(n)
    integer, len :: n = 2
    character(len=2*n) :: c1
    character(len=n*n - 1) :: c2
  end type
  type(t(3)) :: p
  if (len (p%c1) /= 6) stop 1
! Add a slightly more complicated version and use LEN in a different context.
  p%c2 = repeat ('X', len (p%c2))
  if (p%c2 /= 'XXXXXXXX') stop 2
end program
