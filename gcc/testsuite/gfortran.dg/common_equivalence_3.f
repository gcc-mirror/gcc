! { dg-do compile }
! PR fortran/18870
!
      program main
      equivalence (a,c)
      equivalence (b,c)
      common /foo/ a
      common /bar/ b ! { dg-error "equivalenced to another COMMON" }
      c=3.
      print *,a
      print *,b
      end


