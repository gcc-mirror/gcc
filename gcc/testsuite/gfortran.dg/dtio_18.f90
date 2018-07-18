! { dg-do compile }
!
! PR 78592: [7 Regression] ICE in gfc_find_specific_dtio_proc, at fortran/interface.c:4939
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

program p
   type t
   end type
   type(t) :: z
   interface write(formatted)
      module procedure wf    ! { dg-error "is neither function nor subroutine" }
   end interface
   print *, z
end
