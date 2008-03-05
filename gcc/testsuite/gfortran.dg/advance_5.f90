! { dg-do run { target fd_truncate } }
! PR31207 Last record truncated for read after short write.
character(len=20) :: b
! write something no advance
open(10,file="fort.10",position="rewind")
write(10, '(a,t1,a)',advance='no') 'xxxxxx', 'abc'
close(10)
! append some data
open(10,file="fort.10",position="append")
write(10, '(a)') 'def'
close(10)
! check what is in the first record
open(10,file="fort.10",position="rewind")
read(10,'(a)') b
close(10, status="delete")
if (b.ne."abcxxx") call abort()
end
