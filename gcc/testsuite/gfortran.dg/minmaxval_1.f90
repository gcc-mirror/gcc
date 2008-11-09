! { dg-do compile }
! Tests the fix for PR37836 in which the specification expressions for
! y were not simplified because there was no simplifier for minval and
! maxval.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
! nint(exp(3.0)) is equal to 20 :-)
!
      function fun4a()
         integer fun4a
         real y(minval([25, nint(exp(3.0)), 15]))

        fun4a = size (y, 1)
       end function fun4a

      function fun4b()
         integer fun4b
         real y(maxval([25, nint(exp(3.0)), 15]))
         save

         fun4b = size (y, 1)
      end function fun4b

      EXTERNAL fun4a, fun4b
      integer fun4a, fun4b
      if (fun4a () .ne. 15) call abort 
      if (fun4b () .ne. 25) call abort 
      end
