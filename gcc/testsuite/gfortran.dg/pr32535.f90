! { dg-do compile }
!
! PR32535: namelist with private items contained in sub-sub-procedure of a module rejected
!
! Contributed by Janus Weil <jaydub66@gmail.com> 

module mo
implicit none
real, private:: a,b,c

contains

  subroutine sub
    implicit none
    namelist /nl1/ a,b,c

    contains

    subroutine subsub
      implicit none
      namelist /nl2/ a,b,c
    end subroutine subsub
  end subroutine sub
end module mo

! { dg-final { cleanup-modules "mo" } }
