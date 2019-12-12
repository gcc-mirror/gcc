! { dg-do compile }
! PR fortran/91564
! Contributed by Gerhard Steinmetz.
program p
   integer i, j
   call kill (1, 2, 3)        ! { dg-error "shall be an INTENT" }
   i = 42
   call bar(i, j)
end

subroutine bar(n, m)
   integer, intent(in) :: n
   integer, intent(inout) :: m
   call kill (1, 3, n)        ! { dg-error "shall be an INTENT" }
   call kill (1, 3, m)
end subroutine bar
