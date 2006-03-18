! { dg-do run }
! PR26509 : Writing beyond fixed length direct access records.
! Test case derived from PR.
! Submitted  by Jerry Delisle <jvdelisle@gcc.gnu.org>.
program testrecl
      implicit none
      open(unit = 10, form = 'unformatted', access = 'direct', recl = 4)
      write(unit=10,rec=1, err=100) 1d0
      call abort()
 100  continue
      close(unit=10, status='delete')
      end
