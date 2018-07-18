! { dg-do compile }
! { dg-options "-std=gnu" }
!
! Test of fix (patch unknown) for pr19181 and pr21300. This test is based
! on the example given in 21300.  Note that this can be executed.
!
! Contributed by Paul Thomas  <pault@gnu.org>
!
  TYPE ast_obs
    real, DIMENSION(:), POINTER :: geopos
  END TYPE ast_obs

  TYPE(ast_obs), PARAMETER    :: undefined_ast_obs = AST_OBS(NULL())
  type(ast_obs)               :: my_ast_obs
  real, target, dimension(10) :: rt

  my_ast_obs%geopos => rt
  if (.not.associated (my_ast_obs%geopos)) STOP 1

  call get_null_ast_obs (my_ast_obs)
  if (associated (my_ast_obs%geopos)) STOP 2

CONTAINS

  SUBROUTINE get_null_ast_obs (obs1)
    TYPE(ast_obs)  :: obs1
    obs1 = undefined_ast_obs
    RETURN
  END SUBROUTINE get_null_ast_obs

END

