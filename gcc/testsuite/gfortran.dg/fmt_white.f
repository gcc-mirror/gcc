! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR24268 Test case derived from example given by Iwan Kawrakow
! Embedded spaces in format strings should be ignored.
! Prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      program pr24268
      real x
      character*13 line
      line = "12.34"
      read(line,*) x
      write(line,10) x
 10                                                            format(g1
     *      1.4)
      if (line.ne."  12.34") call abort()
      line = ""
      write(line,20) x
 20   format(t r 2 , g 1 1 . 4)
      if (line.ne."    12.34") call abort()
      end
