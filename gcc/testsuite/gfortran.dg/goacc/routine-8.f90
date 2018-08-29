! Test ACC ROUTINE inside an interface block.

program main
  interface
     function s_1 (a)
       integer a
       !$acc routine
     end function s_1
  end interface

  interface
     function s_2 (a)
       integer a
       !$acc routine seq
     end function s_2
  end interface

  interface
     function s_3 (a)
       integer a
       !$acc routine (s_3) ! { dg-error "Only the ..ACC ROUTINE form without list is allowed in interface block" }
     end function s_3
  end interface

  interface
     function s_4 (a)
       integer a
         !$acc routine (s_4) seq ! { dg-error "Only the ..ACC ROUTINE form without list is allowed in interface block" }
     end function s_4
  end interface
end program main

