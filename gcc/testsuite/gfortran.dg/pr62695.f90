! { dg-do compile }
! { dg-options "-O -fipa-pta" }

MODULE dbcsr_dist_operations
  TYPE dbcsr_mp_obj
  END TYPE dbcsr_mp_obj
  INTERFACE
    SUBROUTINE dbcsr_mp_new(mp_env, pgrid, mp_group, mynode, numnodes, myprow,&
         mypcol)
      IMPORT
      TYPE(dbcsr_mp_obj), INTENT(OUT)          :: mp_env
      INTEGER, DIMENSION(0:, 0:), INTENT(IN)   :: pgrid
    END SUBROUTINE dbcsr_mp_new
  END INTERFACE
CONTAINS
  SUBROUTINE dbcsr_mp_make_env (mp_env, mp_group, &
       nprocs, pgrid_dims, error)
    TYPE(dbcsr_mp_obj), INTENT(OUT)          :: mp_env
      OPTIONAL                               :: pgrid_dims
    INTEGER                                  :: error_handle, group, mynode, &
                                                numnodes, pcol, prow
    INTEGER, ALLOCATABLE, DIMENSION(:, :)    :: pgrid
    INTEGER, DIMENSION(2)                    :: coord, myploc, npdims
    CALL dbcsr_mp_new (mp_env, pgrid, group, mynode, numnodes,&
         myprow=myploc(1), mypcol=myploc(2))
  END SUBROUTINE dbcsr_mp_make_env
END MODULE dbcsr_dist_operations
