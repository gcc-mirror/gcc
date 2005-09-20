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
      if (i (8).ne.8) call abort
      if (i (4).ne.6) call abort
      if (j (0).ne.3) call abort
      if (j (7).ne.7) call abort
      end
