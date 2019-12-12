! { dg-do compile }
! Test the fix for PR43450 in which the use of 'replica_env_type'
! caused an ICE in ep_types
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
MODULE replica_types
  TYPE replica_env_type
  END TYPE replica_env_type
CONTAINS
  SUBROUTINE rep_env_create(rep_env, para_env, input, nrep, prep,&
       sync_v,keep_wf_history,row_force)
  END SUBROUTINE rep_env_create
  SUBROUTINE rep_envs_add_rep_env(rep_env)
    TYPE(replica_env_type), POINTER          :: rep_env
  END SUBROUTINE rep_envs_add_rep_env
END MODULE replica_types
MODULE ep_types
  USE replica_types
  TYPE ep_env_type
     TYPE(replica_env_type), POINTER :: mol_envs
  END TYPE ep_env_type
  TYPE ep_env_p_type
     TYPE(ep_env_type), POINTER :: ep_env
  END TYPE ep_env_p_type
  TYPE(ep_env_p_type), DIMENSION(:), POINTER :: ep_envs
CONTAINS
  SUBROUTINE ep_force_release()
  END SUBROUTINE ep_force_release
END MODULE ep_types
