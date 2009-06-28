! { dg-do compile }
!
! PR fortran/34112
!
! Check for calling convention consitency
! in procedure-pointer assignments.

program test
  interface
    subroutine sub1()
    end subroutine sub1
    subroutine sub2()
      !GCC$ ATTRIBUTES CDECL :: sub2
    end subroutine sub2
    subroutine sub3()
      !GCC$ ATTRIBUTES STDCALL :: sub3
    end subroutine sub3
    subroutine sub4()
!GCC$ ATTRIBUTES FASTCALL :: sub4
    end subroutine sub4
  end interface

  !gcc$ attributes cdecl :: cdecl
  !gcc$ attributes stdcall :: stdcall
  procedure(), pointer :: ptr
  procedure(), pointer :: cdecl
  procedure(), pointer :: stdcall
  procedure(), pointer :: fastcall
  !gcc$ attributes fastcall :: fastcall

  ! Valid:
  ptr => sub1
  cdecl => sub2
  stdcall => sub3
  fastcall => sub4

  ! Invalid:
  ptr => sub3 ! { dg-error "mismatch in the calling convention" }
  ptr => sub4 ! { dg-error "mismatch in the calling convention" }
  cdecl => sub3 ! { dg-error "mismatch in the calling convention" }
  cdecl => sub4 ! { dg-error "mismatch in the calling convention" }
  stdcall => sub1 ! { dg-error "mismatch in the calling convention" }
  stdcall => sub2 ! { dg-error "mismatch in the calling convention" }
  stdcall => sub4 ! { dg-error "mismatch in the calling convention" }
  fastcall => sub1 ! { dg-error "mismatch in the calling convention" }
  fastcall => sub2 ! { dg-error "mismatch in the calling convention" }
  fastcall => sub3 ! { dg-error "mismatch in the calling convention" }
end program
