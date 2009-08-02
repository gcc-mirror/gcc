c { dg-do compile }
c { dg-options "-std=legacy" }
c
c     SEGVs in loop.c with -O2.

      character*80 function nxtlin(lun,ierr,itok)
      character onechr*1,twochr*2,thrchr*3
      itok=0
      do while (.true.)
         read (lun,'(a)',iostat=ierr) nxtlin
         if (nxtlin(1:1).ne.'#') then
            ito=0
            do 10 it=1,79
               if (nxtlin(it:it).ne.' ' .and. nxtlin(it+1:it+1).eq.' ')
     $              then
                  itast=0
                  itstrt=0
                  do itt=ito+1,it
                     if (nxtlin(itt:itt).eq.'*') itast=itt
                  enddo
                  itstrt=ito+1
                  do while (nxtlin(itstrt:itstrt).eq.' ')
                     itstrt=itstrt+1
                  enddo
                  if (itast.gt.0) then
                     nchrs=itast-itstrt
                     if (nchrs.eq.1) then
                        onechr=nxtlin(itstrt:itstrt)
                        read (onechr,*) itokn
                     elseif (nchrs.eq.2) then
                        twochr=nxtlin(itstrt:itstrt+1)
                        read (twochr,*) itokn
                     elseif (nchrs.eq.3) then
                        thrchr=nxtlin(itstrt:itstrt+2)
                        read (thrchr,*) itokn
                     elseif (nchrs.eq.4) then
                        thrchr=nxtlin(itstrt:itstrt+3)
                        read (thrchr,*) itokn
                     endif
                     itok=itok+itokn
                  else
                     itok=itok+1
                  endif
                  ito=it+1
               endif
 10         continue
            return
         endif
      enddo
      return
      end
