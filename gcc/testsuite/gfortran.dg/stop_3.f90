! { dg-do compile }
! { dg-options "-std=f2003" }
! F95 and F2003 do not require a blank after STOP

  implicit none
  integer,      parameter :: p = 99
  character(*), parameter :: s = "stopp"
  stop1
  stop2!
  stop3;stop4!
  stopp
  stop&!
       &;stop;&!
       stop&!
       s&
       ;stop"x";&!
       ;st&!
       &op&!
       p
  stops
  stop"last " // s
end
