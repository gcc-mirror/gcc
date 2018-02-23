! { dg-do run }
! Tests the patch to implement the array version of the TRANSFER
! intrinsic (PR17298).

! test the PR is fixed.

   call test1 ()

contains

   subroutine test1 ()
     complex(4) :: z = (1.0, 2.0)
     real(4) :: cmp(2), a(4, 4)
     integer(2) :: it(4, 2, 4), jt(32)

! The PR testcase.

     cmp = transfer (z, cmp) * 2.0
     if (any (cmp .ne. (/2.0, 4.0/))) STOP 1

   end subroutine test1

end
