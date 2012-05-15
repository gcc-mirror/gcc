! { dg-do compile }
! Test the fix for PR42481, in which 'sub' was not recognised as
! a generic interface.
!
! Contributed by William Mitchell < william.mitchell@nist.gov>
!
module mod1
contains
  subroutine sub(x, chr)
    real x
    character(8) chr
    if (trim (chr) .ne. "real") call abort
    if (int (x) .ne. 1) call abort
  end subroutine sub
end module mod1

module mod2
  use mod1
  interface sub
    module procedure sub, sub_int
  end interface sub
contains
  subroutine sub_int(i, chr)
    character(8) chr
    integer i
    if (trim (chr) .ne. "integer") call abort
    if (i .ne. 1) call abort
  end subroutine sub_int
end module mod2

program prog
  use mod1
  use mod2
  call sub(1, "integer ")
  call sub(1.0, "real    ")
end program prog
