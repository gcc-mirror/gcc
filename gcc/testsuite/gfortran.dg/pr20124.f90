! { dg-do run }
! pr 20124
      character*80 line
      x = -.01
      y = .01
      write(line,'(2f10.2)') x, y
      if (line.ne.'     -0.01      0.01') call abort
      end
