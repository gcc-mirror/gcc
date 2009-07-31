      subroutine foo(a)
      logical bar
      dimension a(12,2)
      dimension b(12,8)
      if(cd .eq. 1) then
         if (bar) write(iw,*) norb
         if(ef.ne.1) then
            do i=1,norb
            end do
         end if
      end if
         do 400 j = 1,8
            b(i,j) = 0
  400    continue
         do 410 j=1,norb
            a(i,j) = 0
  410    continue
      call rdrsym(b)
      end
