C Test for bug in reg-stack handling conditional moves.
C Reported by Tim Prince <tprince@computer.org>
C
C { dg-do run { target { { i[6789]86-*-* x86_64-*-* } && ilp32 } } }
C { dg-options "-ffast-math -march=pentiumpro" }

      double precision function foo(x, y)
         implicit none
         double precision x, y
         double precision a, b, c, d
         if (x /= y) then
             if (x * y >= 0) then
                 a = abs(x)
                 b = abs(y)
                 c = max(a, b)
                 d = min(a, b)
                 foo = 1 - d/c
             else       
                 foo = 1
             end if  
         else
             foo = 0
         end if
      end

      program test
      implicit none

      integer ntests
      parameter (ntests=7)
      double precision tolerance
      parameter (tolerance=1.0D-6)

C Each column is a pair of values to feed to foo,
C and its expected return value.
      double precision a(ntests), b(ntests), x(ntests)
      data a /1, -23, -1,   1,   9,  10,  -9/
      data b /1, -23, 12, -12,  10,   9, -10/
      data x /0,   0,  1,   1, 0.1, 0.1, 0.1/

      double precision foo
      double precision result
      integer i

      do i = 1, ntests
         result = foo(a(i), b(i))
         if (abs(result - x(i)) > tolerance) then
           print *, i, a(i), b(i), x(i), result
           call abort
         end if
      end do
      end
