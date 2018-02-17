! { dg-do run }     
! PR25349 Check T editing. Test case from PR submitted by Thomas Koenig
! Contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>
      program main
      character(len=10) line
      write (line,'(1X,A,T1,A)') 'A','B'
      if (line.ne.'BA') STOP 1
      end
