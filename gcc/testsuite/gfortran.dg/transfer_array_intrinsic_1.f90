! { dg-do run { target i?86-*-* x86_64-*-* } }
! Tests the patch to implement the array version of the TRANSFER
! intrinsic (PR17298).
! Contributed by Paul Thomas  <pault@gcc.gnu.org>

   character(8) :: ch(2) = (/"lmnoPQRS","LMNOpqrs"/)

! tests numeric transfers(including PR testcase).

   call test1 ()

! tests numeric/character transfers.

   call test2 ()

! Test dummies, automatic objects and assumed character length.

   call test3 (ch, ch, ch, 8)

contains

   subroutine test1 ()
     complex(4) :: z = (1.0, 2.0)
     real(4) :: cmp(2), a(4, 4)
     integer(2) :: it(4, 2, 4), jt(32)

! The PR testcase.

     cmp = transfer (z, cmp) * 2.0
     if (any (cmp .ne. (/2.0, 4.0/))) call abort ()

! Check that size smaller than the source word length is OK.

     z = (-1.0, -2.0)
     cmp = transfer (z, cmp, 1) * 8.0
     if (any (cmp .ne. (/-8.0, 4.0/))) call abort ()

! Check multi-dimensional sources and that transfer works as an actual
! argument of reshape.

     a = reshape ((/(rand (), i = 1, 16)/), (/4,4/))
     jt = transfer (a, it)
     it = reshape (jt, (/4, 2, 4/))
     if (any (reshape (transfer (it, a), (/4,4/)) .ne. a)) call abort ()

   end subroutine test1

   subroutine test2 ()
     integer(4) :: y(4), z(2)
     character(4) :: ch(4)
     y = (/(i + ishft (i + 1, 8) + ishft (i + 2, 16) &
              + ishft (i + 3, 24), i = 65, 80 , 4)/)

! Check source array sections in both directions.

     ch = "wxyz"
     ch = transfer (y(2:4:2), ch)
     if (any (ch .ne. (/"EFGH","MNOP","wxyz","wxyz"/))) call abort ()
     ch = "wxyz"
     ch = transfer (y(4:2:-2), ch)
     if (any (ch .ne. (/"MNOP","EFGH","wxyz","wxyz"/))) call abort ()

! Check that a complete array transfers with size absent.

     ch = transfer (y, ch)
     if (any (ch .ne. (/"ABCD","EFGH","IJKL","MNOP"/))) call abort ()

! Check that a character array section is OK

     z = transfer (ch(2:3), y)
     if (any (z .ne. y(2:3))) call abort ()

! Check dest array sections in both directions.

     ch = "wxyz"
     ch(3:4) = transfer (y, ch, 2)
     if (any (ch .ne. (/"wxyz","wxyz","ABCD","EFGH"/))) call abort ()
     ch = "wxyz"
     ch(3:2:-1) = transfer (y, ch, 3)
     if (any (ch .ne. (/"wxyz","EFGH","ABCD","wxyz"/))) call abort ()

! Check that too large a value of size is cut off.

     ch = "wxyz"
     ch(1:2) = transfer (y, ch, 3)
     if (any (ch .ne. (/"ABCD","EFGH","wxyz","wxyz"/))) call abort ()

! Make sure that character to numeric is OK.

     z = transfer (ch, y)
     if (any (y(1:2) .ne. z)) call abort ()

   end subroutine test2

   subroutine test3 (ch1, ch2, ch3, clen)
     integer clen
     character(8) :: ch1(:)
     character(*) :: ch2(2)
     character(clen) :: ch3(2)
     character(8) :: cntrl(2) = (/"lmnoPQRS","LMNOpqrs"/)
     integer(8) :: ic(2)
     ic = transfer (cntrl, ic)

! Check assumed shape.

     if (any (ic .ne. transfer (ch1, ic))) call abort ()

! Check assumed character length.

     if (any (ic .ne. transfer (ch2, ic))) call abort ()

! Check automatic character length.

     if (any (ic .ne. transfer (ch3, ic))) call abort ()

  end subroutine test3

end
