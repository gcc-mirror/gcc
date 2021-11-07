/* PR middle-end/102972 */

#ifdef __cplusplus
extern "C" {
#endif

/* From omp.h  */
extern int omp_get_num_teams (void);
extern void omp_set_num_teams (int);
extern int omp_get_team_size (int);
extern int omp_get_team_num (void);
extern int omp_get_max_teams (void);
extern void omp_set_teams_thread_limit (int);
extern int omp_get_teams_thread_limit (void);
extern int omp_is_initial_device (void);
extern int omp_get_num_threads (void);


#ifdef __cplusplus
}
#endif


void valid ()
{
  #pragma omp teams
  {
    #pragma omp distribute
    for (int i = 0; i < 64; i++)
      ;

    int n = omp_get_num_teams ();
    if (n >= omp_get_team_num ())
      __builtin_abort ();

    #pragma omp parallel for
    for (int i = 0; i < 64; i++)
      if (!omp_is_initial_device () || omp_get_num_threads () < 0)
	__builtin_abort ();

    #pragma omp loop
    for (int i = 0; i < 64; i++)
      ;
  }
}

void invalid_nest ()
{
  #pragma omp teams
  {
    #pragma distribute parallel for simd
    for (int i = 0; i < 64; i++)
      ;

    int n = 0;
    n += omp_get_team_size (0);  /* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_team_size\[^\n\r]*' strictly nested in a 'teams' region" }  */
    n += omp_get_num_teams ();
    n += omp_get_team_num ();
    omp_set_num_teams (n);  /* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_set_num_teams\[^\n\r]*' strictly nested in a 'teams' region" }  */
    n += omp_get_max_teams ();  /* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_max_teams\[^\n\r]*' strictly nested in a 'teams' region" }  */
    n += omp_get_teams_thread_limit ();  /* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_teams_thread_limit\[^\n\r]*' strictly nested in a 'teams' region" }  */
    omp_set_teams_thread_limit (n);  /* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_set_teams_thread_limit\[^\n\r]*' strictly nested in a 'teams' region" }  */
  }
}
