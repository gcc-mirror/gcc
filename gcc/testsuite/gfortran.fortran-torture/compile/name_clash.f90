! This is the testcase from PR13249.
! the two different entities named AN_EXAMPLE shouldn't conflict
  MODULE MOD
  INTEGER FOO
  END
  PROGRAM MAIN
  USE MOD
  COMMON /FOO/ BAR
  END
