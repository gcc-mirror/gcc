! This is the testcase from PR13249.
! the two different entities named AN_EXAMPLE shouldn't conflict
! the real bug is PR15481, we currently (2004/06/09) work around PR13249
  MODULE MOD
  INTEGER FOO
  END
  PROGRAM MAIN
  USE MOD
  COMMON /FOO/ BAR
  END
