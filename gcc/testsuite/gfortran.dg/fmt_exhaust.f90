! { dg-do run { target fd_truncate } }
! PR27304 Test running out of data descriptors with data remaining.
! Derived from case in PR.  Submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>.
      program test
      implicit none
      integer :: n
      n = 1
      write(10,"(i7,(' abcd'))", err=10) n, n
      call abort()
 10   close(10, status="delete")
      end program test
