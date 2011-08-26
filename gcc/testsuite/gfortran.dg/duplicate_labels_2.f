! { dg-do compile }
!
! PR fortran/50071
! Duplicate statement labels should not be rejected if they appear in
! different scoping units
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

c gfortran complains about duplicate statement labels
c this is a legal program because types have their own scoping units 
c and you may have same labels in different scoping units,
c as you may have same identifiers inside, like G.
      type t1
1      integer G
      end type
      type t2
1      integer G
      end type
c this is legal
      goto 1
      print *,'bad'
1     continue
      print *,'good'
      end

