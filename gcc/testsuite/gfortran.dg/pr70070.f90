! { dg-do compile }
! PR fortran/70070 - ICE on initializing character data beyond min/max bound

program p
  character(1) :: a, b
  data (a(i:i),i=0,0) /1*'#'/   ! { dg-error "Substring start index" }
  data (b(i:i),i=2,3) /2*'#'/   ! { dg-error "Substring end index" }
end
