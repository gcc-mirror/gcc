! { dg-do run }
! PR fortran/91359
! Orginal code contributed by Brian T. Carcich <briantcarcich at gmail dot com>
!
      logical function zero()
         goto 2
1        return
2        zero = .false.
         if (.not.zero) goto 1
         return
      end

      program test_zero
         logical zero
         if (zero()) stop 1
      end
