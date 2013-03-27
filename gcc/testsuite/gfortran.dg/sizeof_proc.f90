! { dg-do compile }
!
! PR 47023: C_Sizeof: Rejects valid code
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

use iso_c_binding
procedure(real) :: proc
procedure(real), pointer :: pp
pp => sin

print *,sizeof(proc)    ! { dg-error "shall not be a procedure" }
print *,sizeof(pp)      ! { dg-error "shall not be a procedure" }
print *,sizeof(pp(0.))
print *,sizeof(sub)     ! { dg-error "shall not be a procedure" }
print *,sizeof(func)    ! { dg-error "shall not be a procedure" }
print *,sizeof(func())

contains

  subroutine sub
  end subroutine

  real function func()
    func = 0.
  end function

end
