! { dg-do run }
      program test
        j = 0
        do 10 i=1,3
           if(i == 2) goto 10 ! { dg-warning "" "" }
           j = j+1
10      enddo
        if (j/=2) call abort
      end
