* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system,
* including when initial values are provided (e.g. DATA).

      program test
      implicit none

      character c
      double precision d
      common /cmn/ c, d

      if (c .ne. '1') call abort
      if (d .ne. 10.) call abort

      end

      block data init
      implicit none

      character c
      double precision d
      common /cmn/ c, d

      data c/'1'/, d/10./

      end
