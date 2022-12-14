! { dg-do compile }
! PR fortran/106911 - ICE in gfc_convert_mpz_to_signed
! Contributed by G.Steinmetz

program p
  implicit none
  integer, parameter :: a = 10
  integer, parameter :: b = 20
  integer, parameter :: c = ishftc(1_1, a, b) ! { dg-error "must be less than or equal" }
  integer, parameter :: d = ishftc(1_1, a, 0) ! { dg-error "must be positive" }
  interface
     subroutine s
       import :: a, b
       integer, parameter :: e = ishftc(1_1, a, b) ! { dg-error "must be less than or equal" }
       integer, parameter :: f = ishftc(1_1, a, 0) ! { dg-error "must be positive" }
     end
  end interface
end
