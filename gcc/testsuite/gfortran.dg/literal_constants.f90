! { dg-do compile }
! { dg-options "-ffree-form" }
! PR fortran/92805 - blanks within literal constants in free-form mode

      implicit none
      integer, parameter :: ck = kind ("a")  ! default character kind
      integer, parameter :: rk = kind (1.0)  ! default real kind
      print *, 1_"abc"
      print *, 1 _"abc"   ! { dg-error "Syntax error" }
      print *, 1_ "abc"   ! { dg-error "Missing kind-parameter" }
      print *, 1 _ "abc"  ! { dg-error "Syntax error" }
      print *, ck_"a"
      print *, ck _"ab"   ! { dg-error "Syntax error" }
      print *, ck_ "ab"   ! { dg-error "Syntax error" }
      print *, ck _ "ab"  ! { dg-error "Syntax error" }
      print *, 3.1415_4
      print *, 3.1415 _4  ! { dg-error "Syntax error" }
      print *, 3.1415_ 4  ! { dg-error "Missing kind-parameter" }
      print *, 3.1415 _ 4 ! { dg-error "Syntax error" }
      print *, 3.1415_rk
      print *, 3.1415 _rk ! { dg-error "Syntax error" }
      print *, 3.1415_ rk ! { dg-error "Missing kind-parameter" }
      print *, 3.141 _ rk ! { dg-error "Syntax error" }
      end
