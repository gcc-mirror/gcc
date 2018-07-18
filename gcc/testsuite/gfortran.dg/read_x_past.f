! { dg-do run }
! { dg-options -w }
! PR 26661 : Test reading X's past file end with no LF or CR.
! PR 26880 : Tests that rewind clears the gfc_unit read_bad flag.
! PR 43265 : Tests that no error occurs with or without X at end.
! Contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>.
      implicit none
      character(3) a(4)
      integer i
      open (10, status="scratch")
 10   format(A,$)  ! This is not pedantic
      write(10,10)' abc def ghi jkl'
      rewind(10)

      a = ""
      read(10,20)(a(i),i=1,4)
      if (a(4).ne."jkl") STOP 1

      rewind(10)

      a = ""
      read(10,30)(a(i),i=1,4)
      if (a(4).ne."jkl") STOP 2

 20   format(1x,a3,1x,a3,1x,a3,1x,a3,10x)
 30   format(1x,a3,1x,a3,1x,a3,1x,a3)
      close(10)
      end
