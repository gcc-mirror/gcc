! { dg-do run }
! PR 16907 : We didn't support REAL array indices as an extension
       integer I, A(10)
       A = 2
       I=A(1.0) ! { dg-warning "Extension" }
       if (i/=2) STOP 1
       end
