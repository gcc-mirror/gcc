! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR25631 Check that TL editing works for special case of no bytes written yet.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      real x
      character*15 line
      x = 12.34
      write(line,10) x
 10   format(tr2,tl2,g11.4)
      if (line.ne.'  12.34        ') STOP 1
      write(line,20) x
 20   format(tr5,tl3,g11.4)
      if (line.ne.'    12.34      ') STOP 2
      write(line,30) x
 30   format(tr5,tl3,tl3,g11.4)
      if (line.ne.'  12.34        ') STOP 3
      write(line,40) x
 40   format(tr25,tl35,f11.4)
      if (line.ne.'    12.3400    ') STOP 4
      write(line,50) x
 50   format(tl5,tr3,f11.4)
      if (line.ne.'       12.3400 ') STOP 5
      write(line,60) x
 60   format(t5,tl3,f11.4)
      if (line.ne.'     12.3400   ') STOP 6
      end
