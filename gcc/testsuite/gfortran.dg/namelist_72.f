! { dg-do run }
!
! PR fortran/49791
!
! Contributed by Elliott Sales de Andrade
!
      program namelist_test

      dimension xpos(5000), ypos(5000)
      namelist /geometry/ xpos, ypos

      xpos = -huge(xpos)
      ypos = -huge(ypos)

      open(unit=4,file='geometry.in')
      write(4,'(a)') '$geometry'
      write(4,'(a)') ' xpos(1)= 0.00, 0.10, 0.20, 0.30, 0.40,'
      write(4,'(a)') ' ypos(1)= 0.50, 0.60, 0.70, 0.80, 0.90,'
      write(4,'(a)') '$end'

      close(4)

      open (unit=4,file='geometry.in',status='old',form='formatted')
      read (4,geometry)
      close(4, status='delete')

      !print *, 'xpos', xpos(1:10), 'ypos', ypos(1:10)

      if (any (xpos(1:5) /= [0.00, 0.10, 0.20, 0.30, 0.40]))STOP 1
      if (any (ypos(1:5) /= [0.50, 0.60, 0.70, 0.80, 0.90]))STOP 2
      if (any (xpos(6:) /= -huge(xpos))) STOP 3
      if (any (ypos(6:) /= -huge(ypos))) STOP 4
      end
