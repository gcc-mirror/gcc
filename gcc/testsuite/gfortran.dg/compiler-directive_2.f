! { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
!
! PR fortran/34112
!
! Check for calling convention consitency
! in procedure-pointer assignments.
!
      subroutine test() ! { dg-error "stdcall and fastcall attributes are not compatible" }
cGCC$ attributes stdcall, fastcall::test
      end subroutine test
