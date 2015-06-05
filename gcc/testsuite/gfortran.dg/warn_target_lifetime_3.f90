! { dg-do compile }
! { dg-options "-Wall" }
!
! PR fortran/55476
!
! Contribued by  Janus Weil
!
subroutine test
  integer, pointer :: p
  integer, target :: t
  p => t
contains
  subroutine sub()               ! { dg-warning "defined but not used" }
    if (p /= 0) return
  end subroutine
end subroutine

module m
  integer, pointer :: p2
contains
  subroutine test
    integer, target :: t2
    p2 => t2 ! { dg-warning "Pointer at .1. in pointer assignment might outlive the pointer target" }
  contains
    subroutine sub()             ! { dg-warning "defined but not used" }
      if (p2 /= 0) return
    end subroutine
  end subroutine
end module m
