! { dg-do run }
! PR47285 G format outputs wrong number of characters.
! Test case prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
       PROGRAM FOO
       character(len=50) :: buffer

       WRITE(buffer,"(G0.5,'<')") -10000.
       if (buffer.ne."-10000.<") STOP 1
       WRITE(buffer,"(G1.5E5,'<')") -10000.
       if (buffer.ne."*<") STOP 2
       WRITE(buffer,"(G2.5E5,'<')") -10000.
       if (buffer.ne."**<") STOP 3
       WRITE(buffer,"(G3.5E5,'<')") -10000.
       if (buffer.ne."***<") STOP 4
       WRITE(buffer,"(G4.5E5,'<')") -10000.
       if (buffer.ne."****<") STOP 5
       WRITE(buffer,"(G5.5E5,'<')") -10000.
       if (buffer.ne."*****<") STOP 6
       WRITE(buffer,"(G6.5E5,'<')") -10000.
       if (buffer.ne."******<") STOP 7
       WRITE(buffer,"(G7.5E5,'<')") -10000.
       if (buffer.ne."*******<") STOP 8
       WRITE(buffer,"(G8.5E5,'<')") -10000.
       if (buffer.ne."********<") STOP 9
       WRITE(buffer,"(G9.5E5,'<')") -10000.
       if (buffer.ne."*********<") STOP 10
       WRITE(buffer,"(G10.5E5,'<')") -10000.
       if (buffer.ne."**********<") STOP 11
       WRITE(buffer,"(G11.5E5,'<')") -10000.
       if (buffer.ne."***********<") STOP 12
       WRITE(buffer,"(G12.5E5,'<')") -10000.
       if (buffer.ne."************<") STOP 13
       WRITE(buffer,"(G13.5E5,'<')") -10000.
       if (buffer.ne."*************<") STOP 14
       WRITE(buffer,"(G14.5E5,'<')") -10000.
       if (buffer.ne."-10000.       <") STOP 15
       WRITE(buffer,"(G15.5E5,'<')") -10000.
       if (buffer.ne." -10000.       <") STOP 16
       WRITE(buffer,"(G16.5E5,'<')") -10000.
       if (buffer.ne."  -10000.       <") STOP 17

       STOP
       END
