c { dg-do run }
c pr 17472
c test namelist handles arrays
c Based on example provided by thomas.koenig@online.de

       integer a(10), ctr
       data a / 1,2,3,4,5,6,7,8,9,10 /
       namelist /ints/ a
       do ctr = 1,10
         if (a(ctr).ne.ctr) STOP 1
       end do
       end
