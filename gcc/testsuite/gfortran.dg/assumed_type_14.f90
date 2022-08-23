! { dg-do compile }
! PR fortran/104573 - ICE in resolve_structure_cons
! Contributed by G.Steinmetz
! Contributed by M.Morin

program p
  type t
  end type
  type(*), parameter :: x = t() ! { dg-error "Assumed type of variable" }
  print *, x
end

subroutine s
  type t
     integer :: a
  end type
  character(3), parameter :: x = t(2) ! { dg-error "Cannot convert" }
  character(3), parameter :: y = x    ! { dg-error "Unclassifiable statement" }
  print *, y
end

! { dg-prune-output "Cannot convert" }
