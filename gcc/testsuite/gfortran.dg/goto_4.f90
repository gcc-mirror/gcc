! { dg-do run }
! PR 17708: Jumping to END DO statements didn't do the right thing
      program test
        j = 0
        do 10 i=1,3
           if(i == 2) goto 10 ! { dg-warning "jumps to END" }
           j = j+1
10      enddo                 ! { dg-warning "jumps to END" }
        if (j/=2) call abort
      end
