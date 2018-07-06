! { dg-do compile }
!
! PR 78618: ICE in gfc_check_rank, at fortran/check.c:3670
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

program p
   character, parameter :: c = char(256,4) ! { dg-error "cannot be converted" }
   if (rank(c) /= 0) STOP 1
end
