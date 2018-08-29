C { dg-do run }
C { dg-options "-std=legacy" }
C
C     PR 30525 - comparisons with padded spaces were done
C     signed.
      program main
      character*2 c2
      character*1 c1, c3, c4
C
C  Comparison between char(255) and space padding
C
      c2 = 'a' // char(255)
      c1 = 'a'
      if (.not. (c2 .gt. c1)) STOP 1
C
C  Comparison between char(255) and space
C
      c3 = ' '
      c4 = char(255)
      if (.not. (c4 .gt. c3)) STOP 2

C
C  Check constant folding
C
      if (.not. ('a' // char(255) .gt. 'a')) STOP 3

      if (.not. (char(255) .gt. 'a')) STOP 4
      end
