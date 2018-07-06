! { dg-do run }
! PR 35995 - ifunction.m4 and ifunction_logical.m4 had a bug
! where zero-sized arguments were not handled correctly.
! Test case provided by Dick Hendrickson, amended by
! Thomas Koenig.

      program try_gf0026_etc

      call       gf0026(  0,  1)
      call       foo   (  0,  1)

      end program

      SUBROUTINE GF0026(nf0,nf1)
      LOGICAL LDA(9)
      INTEGER IDA(NF0,9), iii(9)

      lda = (/ (i/2*2 .eq. I, i=1,9) /)
      LDA = ALL ( IDA .NE. -1000,  1)
      if (.not. all(lda)) STOP 1
      if (.not. all(ida .ne. -1000)) STOP 2

      lda = (/ (i/2*2 .eq. I, i=1,9) /)
      LDA = any ( IDA .NE. -1000,  1)
      print *, lda          !expect FALSE
      if (any(lda)) STOP 3
      print *, any(ida .ne. -1000)   !expect FALSE
      if (any(ida .ne. -1000)) STOP 4

      iii = 137
      iii = count ( IDA .NE. -1000,  1)
      if (any(iii /= 0)) STOP 5
      if (count(ida .ne. -1000) /= 0) STOP 6

      END SUBROUTINE

      subroutine foo (nf0, nf1)
      integer, dimension(9):: res, iii
      integer, dimension(nf0,9) :: ida
      res = (/ (-i, i=1,9) /)
      res = product (ida, 1)
      if (any(res /= 1)) STOP 7
      end subroutine foo
