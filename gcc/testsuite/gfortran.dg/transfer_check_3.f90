! { dg-do compile }
! { dg-options "-Wsurprising" }
!
! PR fortran/53691
! PR fortran/53685
!
! TRANSFER checks


! (a) PR 53691
! Failed for -Wsurprising with an ICE as SIZE was assumed to be constant

       SUBROUTINE CGBRFSX(N, RWORK)
         INTEGER N
         REAL RWORK(*)
         REAL ZERO
         PARAMETER (ZERO = 0.0E+0)
         call foo(TRANSFER (RWORK(1:2*N), (/ (ZERO, ZERO) /), N))
       end

! (b) PR 53685
! Failed with a bogus size warning if the source's size is not known at compile
! time (for substrings, the length wasn't set)

      subroutine test(j)
        implicit none
        character(len=4) :: record_type
        integer          :: i, j

        i = transfer (record_type, i)      ! no warning
        i = transfer (record_type(1:4), i) ! gave a warning
        i = transfer (record_type(1:j), i) ! gave a warning
      end
