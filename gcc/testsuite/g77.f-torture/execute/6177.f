      program pr6177
C
C Test case for PR optimization/6177.
C This bug (an ICE) originally showed up in file cblat2.f from LAPACK.
C
      complex x
      complex w(1)
      intrinsic conjg
      x = (2.0d0, 1.0d0)
      w(1) = x
      x = conjg(x)
      w(1) = conjg(w(1))
      if (abs(x-w(1)) .gt. 1.0e-5) call abort
      end
