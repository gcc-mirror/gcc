* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system,
* including when initial values are provided (e.g. DATA).

      program test
      implicit none

      character c
      double precision d(100)
      common /cmn/ c, d

      if (d(80) .ne. 10.) call abort

      end

      block data init
      implicit none

      character c
      double precision d(100)
      common /cmn/ c, d

      data d(80)/10./

      end
