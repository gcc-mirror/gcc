       ! { dg-do compile }
       ! { dg-options "-w" }
       ! Program to test invalid Hollerith constant.
       Program test
       implicit none
       integer i
       i = 0H ! { dg-error "at least one character" }
       i = 4_8H1234 ! { dg-error "should be default" }
       end
