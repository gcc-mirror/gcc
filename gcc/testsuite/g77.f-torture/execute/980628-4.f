* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system,
* including when initial values are provided (e.g. DATA).

      program test
      implicit none

      real r
      double precision d
      common /cmn/ r, d

      if (r .ne. 1.) call abort
      if (d .ne. 10.) call abort

      end

      block data init
      implicit none

      real r
      double precision d
      common /cmn/ r, d

      data r/1./, d/10./

      end
