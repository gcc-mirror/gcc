c     Produced a link error through not eliminating the unused statement
c     function.  It's in `execute' since it needs to link.
      values(i,j) = val((i-1)*n+j)
      end
