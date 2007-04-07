! { dg-do run }
! Test the fix for PR31293.
!
! File: interface4.f90
! http://home.comcast.net/%7Ekmbtib/Fortran_stuff/interface4.f90
! Public domain 2004 James Van Buskirk
! Second attempt to actually create function with LEN
! given by specification expression via function name,
! and SIZE given by specification expression via
! result name.

! g95 12/18/04: Error: Circular specification in variable 'r'.
! ISO/IEC 1539-1:1997(E) section 512.5.2.2:
! "If RESULT is specified, the name of the result variable
! of the function is result-name, its characteristics
! (12.2.2) are those of the function result, and..."
! Also from the same section:
! The type and type parameters (if any) of the result of the
! function subprogram may be specified by a type specification
! in the FUNCTION statement or by the name of the result variable
! appearing in a type statement in the declaration part of the
! function subprogram.  It shall not be specified both ways."
! Also in section 7.1.6.2:
! "A restricted expression is one in which each operation is
! intrinsic and each primary is
! ...
! (7) A reference to an intrinsic function that is
! ...
!     (c) the character inquiry function LEN,
! ...
!     and where each primary of the function is
! ...
!     (b) a variable whose properties inquired about are not
!         (i)   dependent on the upper bound of the last
!               dimension of an assumed-shape array.
!         (ii)  defined by an expression that is not a
!               restricted expression
!         (iii) definable by an ALLOCATE or pointer
!               assignment statement."
! So I think there is no problem with the specification of
! the function result attributes; g95 flunks.

! CVF 6.6C3: Error: This name does not have a type, and must
! have an explicit type. [R]
! Clearly R has a type here: the type and type parameters of
! the function result; CVF flunks.

! LF95 5.70f: Type parameters or bounds of variable r may
! not be inquired.
! Again, the type parameters, though not the bounds, of
! variable r may in fact be inquired; LF95 flunks.

module test1
   implicit none
   contains
      character(f (x)) function test2 (x) result(r)
         implicit integer (x)
         dimension r(modulo (len (r) - 1, 3) + 1)
         integer, intent(in) :: x
         interface
            pure function f (x)
               integer, intent(in) :: x
               integer f
            end function f
         end interface
         integer i

         do i = 1, len (r)
            r(:)(i:i) = achar (mod (i, 32) + iachar ('@'))
         end do
      end function test2
end module test1

program test
   use test1
   implicit none
   character(21) :: chr (3)
   chr = "ABCDEFGHIJKLMNOPQRSTU"

   if (len (test2 (10)) .ne. 21) call abort ()
   if (any (test2 (10) .ne. chr)) call abort ()
end program test

pure function f (x)
   integer, intent(in) :: x
   integer f

   f = 2*x+1
end function f
! { dg-final { cleanup-modules "test1" } }
