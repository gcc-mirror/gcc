c { dg-do run }
       i=3
       j=0
       do i=i,5
         j = j+i
       end do
       do i=3,i
         j = j+i
       end do
       if (i.ne.7) call abort()
       print *, i,j
       end
