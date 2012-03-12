! { dg-do compile }
!
! PR fortran/52542
!
! Ensure that the procedure myproc is Bind(C).
!
! Contributed by Mat Cross of NAG
!
interface
  subroutine s() bind(c)
  end subroutine s
end interface
procedure(s) :: myproc
call myproc()
end
! { dg-final { scan-assembler-not "myproc_" } }
