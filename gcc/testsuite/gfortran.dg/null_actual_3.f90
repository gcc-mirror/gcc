! { dg-do compile }
! { dg-options "-fallow-argument-mismatch -w" }
! PR fortran/107576
! Contributed by G.Steinmetz

program p
  implicit none
  interface
     subroutine r(y)
       integer, pointer :: y(:)
     end subroutine r
  end interface
  integer, pointer :: z(:) => null()
  call r(z)
  call s(z)
  call r(null(z))
  call s(null(z)) ! { dg-error "requires an explicit interface" }
end
