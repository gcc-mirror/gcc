! { dg-do compile }
! PR fortran/82049
! Original code contributed by John Harper <john dot harper at vuw dot ac dot nz>
program ice ! f2003
  implicit none
  character(*), parameter:: a = 'ice', b = '*'
  character(*), parameter:: c(2) = [character(len(a)) :: a, b]
  print "(2A4)",adjustr(c)
end program ice
