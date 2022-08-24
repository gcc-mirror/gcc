! { dg-do compile }
! { dg-options "-ffixed-form" }
! PR fortran/92805 - blanks within literal constants in fixed-form mode

      implicit none
      integer, parameter :: ck = kind ("a")  ! default character kind
      integer, parameter :: rk = kind (1.0)  ! default real kind
      print *, 1_"abc"
      print *, 1 _"abc"
      print *, 1_ "abc"
      print *, ck_"a"
      print *, ck _"ab"
      print *, ck_ "ab"
      print *, 3.1415_4
      print *, 3.1415 _4
      print *, 3.1415_ 4
      print *, 3.1415_rk
      print *, 3.1415 _rk
      print *, 3.1415_ rk
      end
