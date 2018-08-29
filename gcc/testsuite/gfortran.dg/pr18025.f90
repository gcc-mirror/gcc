! PR libfortran/18025  <coudert@clipper.ens.fr>
! { dg-do run }
  character(len=80) :: c
  write(c, "('#',F0.2,'#')") 1.23
  if (c /= '#1.23#') STOP 1
  write(c, "('#',F0.2,'#')") -1.23
  if (c /= '#-1.23#') STOP 2
  end
