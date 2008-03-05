! { dg-do run { target fd_truncate } }
! PR31199, test case from PR report.
       program write_write
       character(len=20) :: a,b,c
       write (10,"(a,t1,a,a)") "xxxxxxxxx", "abc", "def"
       write (10,"(a,t1,a)",advance='no') "xxxxxxxxx", "abc"
       write (10,"(a)") "def"
       write (10,"(a)") "abcdefxxx"
       rewind(10)
       read(10,*) a
       read(10,*) b
       read(10,*) c
       if (a.ne.b) call abort()
       IF (b.ne.c) call abort()
       end

