typedef __SIZE_TYPE__ size_t;
typedef struct {
    int disable;
    char *searchconfig[];
} config_t;
typedef struct {
    void *lng;
} arglist_t;
config_t config = {
    .searchconfig = {
		     ((void *) 0)}
};

arglist_t arglist[] = {
    {
     &config.searchconfig[0]}
};
const int arglistsize = ((int) (sizeof(arglist) / sizeof(arglist_t)));
void show_configuration(char *arg)
{
    int i;

    if (!__extension__( {
		       size_t
		       __s1_len,
		       __s2_len; (__builtin_constant_p(arglist[i].lng)
				  && (__s1_len = (!((size_t)
						    (const void *)
						    1)
						  || __s2_len >= 4))
				  ? : (__builtin_constant_p(arglist[i].lng)
				       && ((size_t)
					   (const void *)
					   4)
				       ? (__builtin_constant_p(arg)
					  && ((size_t) (const void *) 1) ?
					  : (__extension__( {
							   __const int * __s2 =
							   (__const int *)
							   (arg);
							   register int
							   __result =
							   (((__const int
							      *) (arglist
								  [i].
								  lng))[0]
							    - __s2[0]);
							   if (__s1_len ==
							       0) {
							   if (__s1_len ==
							       0) {
							   __result =
							   (((__const
							      unsigned char
							      *) (__const
								  char
								  *)
							     (arglist[i].
							      lng))[3] -
							    __s2[3]);}
							   }
							   __result;}
      ))):
				       (__builtin_constant_p(arg)
					?
					(__builtin_constant_p
					 (arglist[i].lng)
					 ? : (__extension__( {
							    char
							    __result
							    =
							    ((__const
							      unsigned *)
							     (arg))[0];
							    if
							    (__s2_len
							     > 0
							     && __result ==
							     0) {
							    if (__s2_len >
								1
								&& __result
								== 0) {
							    }
							    }
							    __result;}
      ))):



					__builtin_strcmp(arglist[i].lng,
							 arg))));}
	))
	return;
}
