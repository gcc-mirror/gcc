! { dg-do compile }
! Tests the fix for PR27554, where the actual argument reference
! to abs would not be recognised as being to an intrinsic
! procedure and would produce junk in the assembler.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org> 
!
      subroutine foo (proc, z)
        external proc
        real proc, z
        if ((proc(z) .ne. abs (z)) .and. 
     &      (proc(z) .ne. alog10 (abs(z)))) call abort ()
        return
      end

        external cos
        interface
          function sin (a)
            real a, sin
          end function sin
        end interface


        intrinsic alog10
        real x
        x = 100.
! The reference here would prevent the actual arg from being seen
! as an intrinsic procedure in the call to foo.
        x = -abs(x)
        call foo(abs, x)
! The intrinsic function can be locally over-ridden by an interface
        call foo(sin, x)
! or an external declaration.
        call foo(cos, x)
! Just make sure with another intrinsic but this time not referenced.
        call foo(alog10, -x)
      end

      function sin (a)
        real a, sin
        sin = -a
        return
      end

      function cos (a)
        real a, cos
        cos = -a
        return
      end
