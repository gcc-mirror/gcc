      subroutine foo(bar)
      real*8 bar(3,3),coefm
      do ii=istart,iend
            do i=1,21
               bar(k,l)=4
            enddo
            do m=1,ne
                  do l=1,3
                     do k=1,l
                     enddo
                     bar(k,l)=bar(k,l)+(v3b-1.d0)
                  enddo
            enddo
            do m=1,ne
               do k=1,l
                  l = l*(v3b**(-coefm))
               enddo
            enddo
      enddo
      end
