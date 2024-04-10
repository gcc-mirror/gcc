! { dg-do run }
program pr111022
  character(20) :: buffer
  write(buffer,"(EN0.3E0)") .6660_4
  if (buffer.ne."666.000E-3") stop 1
  write(buffer,"(EN0.3E0)") 6.660_4
  if (buffer.ne."6.660E+0") stop 2
  write(buffer,"(EN0.3E0)") 66.60_4
  if (buffer.ne."66.600E+0") stop 3
  write(buffer,"(EN0.3E0)") 666.0_4
  if (buffer.ne."666.000E+0") stop 4
  write(buffer,"(EN0.3E0)") 6660.0_4
  if (buffer.ne."6.660E+3") stop 5
  write(buffer,"(EN0.3E0)") 66600.0_4
  if (buffer.ne."66.600E+3") stop 6
  
  write(buffer,"(EN0.0E0)") 666.0_4
  if (buffer.ne."666.E+0") stop 7
  write(buffer,"(EN0.0E1)") 666.0_4
  if (buffer.ne."666.E+0") stop 8
  write(buffer,"(EN0.0E2)") 666.0_4
  if (buffer.ne."666.E+00") stop 9
  write(buffer,"(EN0.0E3)") 666.0_4
  if (buffer.ne."666.E+000") stop 10
  write(buffer,"(EN0.0E4)") 666.0_4
  if (buffer.ne."666.E+0000") stop 11
  write(buffer,"(EN0.0E5)") 666.0_4
  if (buffer.ne."666.E+00000") stop 12
  write(buffer,"(EN0.0E6)") 666.0_4
  if (buffer.ne."666.E+000000") stop 13
  
  write(buffer,"(ES0.3E0)") .6660_4
  if (buffer.ne."6.660E-1") stop 14
  write(buffer,"(ES0.3E0)") 6.660_4
  if (buffer.ne."6.660E+0") stop 15
  write(buffer,"(ES0.3E0)") 66.60_4
  if (buffer.ne."6.660E+1") stop 16
  write(buffer,"(ES0.3E0)") 666.0_4
  if (buffer.ne."6.660E+2") stop 17
  write(buffer,"(ES0.3E0)") 6660.0_4
  if (buffer.ne."6.660E+3") stop 18
  write(buffer,"(ES0.3E0)") 66600.0_4
  if (buffer.ne."6.660E+4") stop 19
  
  write(buffer,"(ES0.0E0)") 666.0_4
  if (buffer.ne."7.E+2") stop 20
  write(buffer,"(ES0.0E1)") 666.0_4
  if (buffer.ne."7.E+2") stop 21
  write(buffer,"(ES0.0E2)") 666.0_4
  if (buffer.ne."7.E+02") stop 22
  write(buffer,"(ES0.0E3)") 666.0_4
  if (buffer.ne."7.E+002") stop 23
  write(buffer,"(ES0.0E4)") 666.0_4
  if (buffer.ne."7.E+0002") stop 24
  write(buffer,"(ES0.0E5)") 666.0_4
  if (buffer.ne."7.E+00002") stop 25
  write(buffer,"(ES0.0E6)") 666.0_4
  if (buffer.ne."7.E+000002") stop 26
  
  write(buffer,"(E0.3E0)") .6660_4
  if (buffer.ne."0.666E+0") stop 27
  write(buffer,"(E0.3)") .6660_4
  if (buffer.ne."0.666E+0") stop 28
  write(buffer,"(E0.1E0)") .6660_4
  if (buffer.ne."0.7E+0") stop 29
  write(buffer,"(E0.1)") .6660_4
  if (buffer.ne."0.7E+0") stop 30
  write(buffer,"(E0.5E0)") .6660_4
  if (buffer.ne."0.66600E+0") stop 31
  write(buffer,"(E0.5)") .6660_4
  if (buffer.ne."0.66600E+0") stop 32
end program pr111022
