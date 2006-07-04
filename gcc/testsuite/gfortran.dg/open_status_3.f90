! { dg-do run }
! PR27704 Incorrect runtime error on multiple OPEN.
! Test case contribyted by Jerry DeLisle <jvdelisle@gcc.gnu.org>
       OPEN(8, FORM = 'unformatted', STATUS = 'scratch')
       OPEN(8, FORM = 'unformatted', status = 'scratch')
       close(8)
       open(8)
       open(8, status = 'old')
       close(8, status="delete")       
       end

