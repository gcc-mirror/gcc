! { dg-do run }
!
! This tests that common blocks function with multiply nested modules.
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
       module mod0
         complex(kind=8) FOO, KANGA
         common /bar/ FOO, KANGA
       contains
         subroutine eyeore ()    
           FOO = FOO + (1.0d0, 0.0d0)
           KANGA = KANGA - (1.0d0, 0.0d0)
         end subroutine eyeore
       end module mod0
       module mod1
         use mod0
         complex ROBIN
         common/owl/ROBIN
       end module mod1
       module mod2
         use mod0
         use mod1
         real(kind=8) re1, im1, re2, im2, re, im
         common /bar/ re1, im1, re2, im2
         equivalence (re1, re), (im1, im)
       contains
         subroutine tigger (w)
           complex(kind=8) w
           if (FOO.ne.(1.0d0, 1.0d0)) call abort ()
           if (KANGA.ne.(-1.0d0, -1.0d0)) call abort ()
           if (ROBIN.ne.(99.0d0, 99.0d0)) CALL abort ()
           if (w.ne.cmplx(re,im)) call abort ()
         end subroutine tigger
       end module mod2

       use mod2
       use mod0, only: w=>foo
       FOO = (0.0d0, 1.0d0)
       KANGA = (0.0d0, -1.0d0)
       ROBIN = (99.0d0, 99.0d0)
       call eyeore ()
       call tigger (w)
       end
