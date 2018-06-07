! { dg-do run }
!
! Test the fix for PR83344.
!
! Contributed by Janne Blomqvist  <jb@gcc.gnu.org>
! and Steve Kargl  <kargl@gcc.gnu.org>
!
program foo
   implicit none
   character(len=1) a
   character(len=2) b
   character(len=3) c
   a = 'a'
   call bah(a, len (a))
   b = 'bb'
   call bah(b, len (b))
   c = 'ccc'
   call bah(c, len (c))
   contains
      subroutine bah(x, clen)
         implicit none
         integer :: clen
         character(len=*), intent(in) :: x
         associate(y => x)
            if (len(y) .ne. clen) stop 1
            if (y .ne. x) stop 2
         end associate
      end subroutine bah
end program foo
