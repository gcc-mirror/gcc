c     Produced a link error through not eliminating the unused statement
c     function after 1998-05-15 change to gcc/toplev.c.  It's in
c     `execute' since it needs to link.
c     Fixed by 1998-05-23 change to f/com.c.
      values(i,j) = val((i-1)*n+j)
      end
