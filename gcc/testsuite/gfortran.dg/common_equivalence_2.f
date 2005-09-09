! { dg-do compile }
! PR fortran/18870
!
      program main
      common /foo/ a
      common /bar/ b
      equivalence (a,c)
      equivalence (b,c) ! { dg-error "indirectly overlap COMMON" }
      c=3.
      print *,a
      print *,b
      end

