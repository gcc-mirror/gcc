! { dg-do compile }
! { dg-options "" }
! Tests the fix for PR22571 in which the derived types in a, b
! c and d were not detected to be different.  In e and f, they
! are the same because they are sequence types.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
subroutine a(p)
  type t
    integer :: t1
  end type
  type(t) :: p
  p%t1 = 42
end subroutine

subroutine b
  type u
    integer :: u1
  end type
  type (u) :: q
  call a(q)  ! { dg-error "Type mismatch" }
  print *, q%u1
end subroutine

subroutine c(p)
  type u
    integer :: u1
  end type
  type(u) :: p
  p%u1 = 42
end subroutine

subroutine d
  type u
    integer :: u1
  end type
  type (u) :: q
  call c(q)  ! { dg-error "Type mismatch" }
  print *, q%u1
end subroutine

subroutine e(p)
  type u
    sequence
    integer :: u1
  end type
  type(u) :: p
  p%u1 = 42
end subroutine

subroutine f
  type u
    sequence
    integer :: u1
  end type
  type (u) :: q
  call e(q)  ! This is OK because the types are sequence.
  print *, q%u1
end subroutine
