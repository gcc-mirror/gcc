! PR libfortran/47757
! { dg-do run { target fortran_large_int } }

  character(kind=4):: str(3,3), s(3)
  str(1,:) = [4_'A', 4_'b', 4_'C']
  str(2,:) = [4_'A', 4_'b', 4_'C']
  str(3,:) = [4_'A', 4_'b', 4_'C']
  s = 4_'A'
  print *, cshift(str, shift=2_16, dim=1_16)
  print *, eoshift(str, shift=2_16, dim=1_16)
  print *, eoshift(str, shift=2_16, boundary=s, dim=1_16)
end
