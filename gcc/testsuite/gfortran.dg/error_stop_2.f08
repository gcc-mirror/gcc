! { dg-do compile }
! PR44371 STOP parsing rejects valid code.
  real, dimension(5,5,5) :: i
  character(1) c, y
  y = 'y'
  read(y,*) c
  if (c=='x') stop; if (c=='X') stop
  if (c=='x') stop size(i); if (c=='X') stop

  if (c=='y') stop size(i) if (c=='Y') stop ! { dg-error "Syntax error in STOP" }
  end
