! { dg-do compile }
!
! PR 49400: [F08] Proc-pointer declaration in BLOCK construct
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

  block
    procedure(real),pointer :: p
  end block
end
