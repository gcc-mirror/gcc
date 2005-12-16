! { dg-do run }
! PR24268 Test case derived from example given by Iwan Kawrakow
! Prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      program pr24268
      real x
      character*11 line
      line = "12.34"
      read(line,*) x
      write(line,10) x
 10                                                            format(g1
     *      1.4)
      if (line.ne."  12.34") call abort()
      end
