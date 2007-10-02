! { dg-do compile }
! Tests the fix for PR33550, in which an ICE would occur, instead of
! the abiguous reference error.
!
! Found at
! http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/1abc1549a6a164f1/
! by James Van Buskirk:
!
module M1
   real x
end module M1

module M2
   contains
      subroutine y
      end subroutine y
end module M2

module M3
   use M2, x => y
end module M3

module M4
   use M1
   use M3
end module M4

module M5
   use M4             ! 'x' is ambiguous here but is not referred to
end module M5

module M6
   use M5             ! ditto
end module M6

program test
   use M1
   use M3
   interface
      function x(z)   ! { dg-error "ambiguous reference" }
      end function x  ! { dg-error "Expecting END INTERFACE" }
   end interface

   write(*,*) 'Hello, world!'
end program test

function x(z)
   x = z
end function x
! { dg-final { cleanup-modules "m1 m2 m3 m4 m5 m6" } }
