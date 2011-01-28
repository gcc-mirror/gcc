! { dg-do run }
! PR47285 G format outputs wrong number of characters.
! Test case prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
       PROGRAM FOO
       character(len=50) :: buffer

       WRITE(buffer,"(G0.5,'<')") -10000.
       if (buffer.ne."-10000.<") call abort
       WRITE(buffer,"(G1.5E5,'<')") -10000.
       if (buffer.ne."*<") call abort
       WRITE(buffer,"(G2.5E5,'<')") -10000.
       if (buffer.ne."**<") call abort
       WRITE(buffer,"(G3.5E5,'<')") -10000.
       if (buffer.ne."***<") call abort
       WRITE(buffer,"(G4.5E5,'<')") -10000.
       if (buffer.ne."****<") call abort
       WRITE(buffer,"(G5.5E5,'<')") -10000.
       if (buffer.ne."*****<") call abort
       WRITE(buffer,"(G6.5E5,'<')") -10000.
       if (buffer.ne."******<") call abort
       WRITE(buffer,"(G7.5E5,'<')") -10000.
       if (buffer.ne."*******<") call abort
       WRITE(buffer,"(G8.5E5,'<')") -10000.
       if (buffer.ne."********<") call abort
       WRITE(buffer,"(G9.5E5,'<')") -10000.
       if (buffer.ne."*********<") call abort
       WRITE(buffer,"(G10.5E5,'<')") -10000.
       if (buffer.ne."**********<") call abort
       WRITE(buffer,"(G11.5E5,'<')") -10000.
       if (buffer.ne."***********<") call abort
       WRITE(buffer,"(G12.5E5,'<')") -10000.
       if (buffer.ne."************<") call abort
       WRITE(buffer,"(G13.5E5,'<')") -10000.
       if (buffer.ne."-10000.      <") call abort
       WRITE(buffer,"(G14.5E5,'<')") -10000.
       if (buffer.ne." -10000.      <") call abort
       WRITE(buffer,"(G15.5E5,'<')") -10000.
       if (buffer.ne."  -10000.      <") call abort
       WRITE(buffer,"(G16.5E5,'<')") -10000.
       if (buffer.ne."   -10000.      <") call abort

       STOP
       END
