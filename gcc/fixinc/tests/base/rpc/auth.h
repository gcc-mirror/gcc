

#if defined( STRUCT_SOCKADDR_CHECK )
struct sockaddr;
extern AUTH* authdes_create( struct sockaddr* );
#endif  /* STRUCT_SOCKADDR_CHECK */


#if defined( SUN_AUTH_PROTO_CHECK )
struct auth_t {
#ifdef __cplusplus
    int (*name)(...); /* C++ bad */
#else
    int (*name)(); /* C++ bad */
#endif
};
#endif  /* SUN_AUTH_PROTO_CHECK */
