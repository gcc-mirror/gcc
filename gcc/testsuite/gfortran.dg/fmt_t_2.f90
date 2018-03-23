! { dg-options "" }
! { dg-do run }
! pr24699, handle end-of-record on READ with T format
! test contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>
      character*132 :: foost1, foost2, foost3
      open (11, status="scratch", action="readwrite")
      write(11, '(a)') "ab cdefghijkl mnop qrst"
      write(11, '(a)') "123456789 123456789 123456789"
      write(11, '(a)') "  Now is the time for all good."
      rewind(11)
      
      read (11, '(a040,t1,040a)', end = 999)  foost1 , foost2
      if (foost1.ne.foost2) STOP 1

      read (11, '(a032,t2,a032t3,a032)', end = 999)  foost1 , foost2, foost3
      if (foost1(1:32).ne."123456789 123456789 123456789   ") STOP 2
      if (foost2(1:32).ne."23456789 123456789 123456789    ") STOP 3
      if (foost3(1:32).ne."3456789 123456789 123456789     ") STOP 4
         
      read (11, '(a017,t1,a0017)', end = 999)  foost1 , foost2
      if (foost1.ne.foost2) STOP 5
      if (foost2(1:17).ne."  Now is the time ") STOP 6
      goto 1000
 999  STOP 7
 1000 continue
      close(11)
      end
