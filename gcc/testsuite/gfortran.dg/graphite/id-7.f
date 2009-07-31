      subroutine dasol(al,au,ad,b,jp,neq,energy)
      real*8 al(*),au(*),ad(*),b(*),zero,energy,bd,dot
      do 100 is=1,neq
         if(b(is).ne.zero) go to 200
 100  continue
      return
 200  if(is.lt.neq) then
      endif
      do 400 j = is,neq
         energy=energy+bd*b(j)
 400  continue
      if(neq.gt.1)then
      endif
      end
