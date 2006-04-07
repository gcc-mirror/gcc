! { dg-do run }
! Tests the patch to implement the array version of the TRANSFER
! intrinsic (PR17298).
! Contributed by Paul Thomas  <pault@gcc.gnu.org>

! Bigendian test posted by Perseus in comp.lang.fortran on 4 July 2005.
! Original had parameter but this fails, at present, if is_gimple_var with -Ox, x>0

   LOGICAL :: bigend
   integer :: icheck = 1

   character(8) :: ch(2) = (/"lmnoPQRS","LMNOpqrs"/)

   bigend = IACHAR(TRANSFER(icheck,"a")) == 0

! tests numeric transfers other than original testscase.

   call test1 ()

! tests numeric/character transfers.

   call test2 ()

! Test dummies, automatic objects and assumed character length.

   call test3 (ch, ch, ch, 8)

contains

   subroutine test1 ()
     real(4) :: a(4, 4)
     integer(2) :: it(4, 2, 4), jt(32)

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

! Allow for endian-ness
     if (bigend) then
       y = (/(i + 3 + ishft (i + 2, 8) + ishft (i + 1, 16) &
                + ishft (i, 24), i = 65, 80 , 4)/)
     else 
       y = (/(i + ishft (i + 1, 8) + ishft (i + 2, 16) &
                + ishft (i + 3, 24), i = 65, 80 , 4)/)
     end if

! Check source array sections in both directions.

     ch = "wxyz"
     ch(1:2) = transfer (y(2:4:2), ch)
     if (any (ch(1:2) .ne. (/"EFGH","MNOP"/))) call abort ()
     ch = "wxyz"
     ch(1:2) = transfer (y(4:2:-2), ch)
     if (any (ch(1:2) .ne. (/"MNOP","EFGH"/))) call abort ()

! Check that a complete array transfers with size absent.

     ch = transfer (y, ch)
     if (any (ch .ne. (/"ABCD","EFGH","IJKL","MNOP"/))) call abort ()

! Check that a character array section is OK

     z = transfer (ch(2:3), y)
     if (any (z .ne. y(2:3))) call abort ()

! Check dest array sections in both directions.

     ch = "wxyz"
     ch(3:4) = transfer (y, ch, 2)
     if (any (ch(3:4) .ne. (/"ABCD","EFGH"/))) call abort ()
     ch = "wxyz"
     ch(3:2:-1) = transfer (y, ch, 2)
     if (any (ch(2:3) .ne. (/"EFGH","ABCD"/))) call abort ()

! Make sure that character to numeric is OK.

     ch = "wxyz"
     ch(1:2) = transfer (y, ch, 2)
     if (any (ch(1:2) .ne. (/"ABCD","EFGH"/))) call abort ()

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
