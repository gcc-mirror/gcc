      subroutine foo(bar)
      dimension bar(100)
      common l_
  50  continue
      do i=1,20
        bar(i)=0
      enddo
      do 100 j=1,l_
        if(sum.gt.r) then
          bar(n2)=j
        end if
 100  continue
      if(bar(4).ne.0) go to 50
      end
