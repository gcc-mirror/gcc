! { dg-do run }
! This checks the patch for PR25395, in which equivalences within one
! segment were broken by indirect equivalences, depending on the
! offset of the variable that bridges the indirect equivalence.
!
! This is a fortran95 version of the original testcase, which was
! contributed by Harald Vogt  <harald.vogt@desy.de>
program check_6
  common /abc/ mwkx(80)
  common /cde/ lischk(20)
  dimension    listpr(20),lisbit(10),lispat(8)
! This was badly compiled in the PR:
  equivalence (listpr(10),lisbit(1),mwkx(10)), &
              (lispat(1),listpr(10))
  lischk = (/0, 0, 0, 0, 0, 0, 0, 0, 0, 1, &
             2, 0, 0, 5, 6, 7, 8, 9,10, 0/)
  call set_arrays (listpr, lisbit)
  if (any (listpr.ne.lischk)) call abort ()
  call sub1
  call sub2
  call sub3
end
subroutine sub1
  common /abc/ mwkx(80)
  common /cde/ lischk(20)
  dimension    listpr(20),lisbit(10),lispat(8)
!     This workaround was OK
  equivalence (listpr(10),lisbit(1)), &
              (listpr(10),mwkx(10)),  &
              (listpr(10),lispat(1))
  call set_arrays (listpr, lisbit)
  if (any (listpr .ne. lischk)) call abort ()
end
!
! Equivalences not in COMMON
!___________________________
! This gave incorrect results for the same reason as in MAIN.
subroutine sub2
  dimension   mwkx(80)
  common /cde/ lischk(20)
  dimension    listpr(20),lisbit(10),lispat(8)
  equivalence (lispat(1),listpr(10)), &
              (mwkx(10),lisbit(1),listpr(10))
  call set_arrays (listpr, lisbit)
  if (any (listpr .ne. lischk)) call abort ()
end
! This gave correct results because the order in which the
! equivalences are taken is different and was given in the PR.
subroutine sub3
  dimension   mwkx(80)
  common /cde/ lischk(20)
  dimension    listpr(20),lisbit(10),lispat(8)
  equivalence (listpr(10),lisbit(1),mwkx(10)), &
              (lispat(1),listpr(10))
  call set_arrays (listpr, lisbit)
  if (any (listpr .ne. lischk)) call abort ()
end
subroutine set_arrays (listpr, lisbit)
  dimension listpr(20),lisbit(10)
  listpr = 0
  lisbit = (/(i, i = 1, 10)/)
  lisbit((/3,4/)) = 0
end
