! PR fortran/23663
      function i (n)
      i = n
      i = max (i, 6)
      return
      entry j (n)
      j = n
      j = max (j, 3)
      end

      program entrytest
      if (i (8).ne.8) STOP 1
      if (i (4).ne.6) STOP 2
      if (j (0).ne.3) STOP 3
      if (j (7).ne.7) STOP 4
      end
