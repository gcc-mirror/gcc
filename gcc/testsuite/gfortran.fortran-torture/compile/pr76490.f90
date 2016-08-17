program membug
call bug1()
end program membug
subroutine unknown(x1,y1,ibig)
   write(*,*)x1,y1,ibig
end subroutine unknown
subroutine bug1()
real arrayq(3000)
   isize=0
   ibig=-1
   x2=0
10 continue
   isize=isize+1
   arrayq(isize)=x2
15 continue
   call unknown(x1,y1,ibig)
   if(ibig.eq.1)then
      goto 10
   elseif(ibig.eq.2)then
      isize=max(1,isize-1)
      goto 15
   endif
end subroutine bug1
