typedef unsigned char	unsigned8;
typedef unsigned short int	unsigned16;
typedef unsigned long int	unsigned32;
typedef char	signed8;
typedef short int	signed16;
typedef long int	signed32;
typedef unsigned32 boolean32;
typedef unsigned long int	error_status_t;
typedef struct {
	unsigned32 time_low;
	unsigned16 time_mid;
	unsigned16 time_hi_and_version;
	unsigned8 clock_seq_hi_and_reserved;
	unsigned8 clock_seq_low;
	unsigned char	node[6];
} uuid_t;

typedef unsigned32 bitset;
typedef signed32 sec_timeval_sec_t;
typedef struct {
	signed32 sec;
	signed32 usec;
} sec_timeval_t;
typedef signed32 sec_timeval_period_t;
typedef signed32 sec_rgy_acct_key_t;

typedef struct {
	uuid_t source;
	signed32 handle;
	boolean32 valid;
} sec_rgy_cursor_t;
typedef unsigned char	sec_rgy_pname_t[257];
typedef unsigned char	sec_rgy_name_t[1025];

typedef signed32 sec_rgy_override_t;
typedef signed32 sec_rgy_mode_resolve_t;
typedef unsigned char	sec_rgy_unix_gecos_t[292];
typedef unsigned char	sec_rgy_unix_login_name_t[1025];
typedef unsigned char	sec_rgy_member_t[1025];
typedef unsigned char	sec_rgy_unix_passwd_buf_t[16];
typedef struct sec_rgy_sid_t {
	uuid_t person;
	uuid_t group;
	uuid_t org;
} sec_rgy_sid_t;
typedef struct {
	signed32 person;
	signed32 group;
	signed32 org;
} sec_rgy_unix_sid_t;
typedef struct {
	sec_rgy_unix_login_name_t name;
	sec_rgy_unix_passwd_buf_t passwd;
	signed32 uid;
	signed32 gid;
	signed32 oid;
	sec_rgy_unix_gecos_t gecos;
	sec_rgy_pname_t homedir;
	sec_rgy_pname_t shell;
} sec_rgy_unix_passwd_t;
typedef unsigned char	sec_rgy_member_buf_t[10250];
typedef struct {
	sec_rgy_name_t name;
	signed32 gid;
	sec_rgy_member_buf_t members;
} sec_rgy_unix_group_t;

typedef struct {
	uuid_t site_id;
	sec_timeval_sec_t person_dtm;
	sec_timeval_sec_t group_dtm;
	sec_timeval_sec_t org_dtm;
} rs_cache_data_t;

typedef enum {
	rs_unix_query_name,
	rs_unix_query_unix_num,
	rs_unix_query_none
} rs_unix_query_t;

typedef struct {
	rs_unix_query_t query;
	union {
		struct {
			long int	name_len;
			sec_rgy_name_t name;
		} name;
		long int	unix_num;
	} tagged_union;
} rs_unix_query_key_t;

static unsigned long int IDL_offset_vec[] =
{
    0,
    sizeof(sec_rgy_unix_group_t),
    (unsigned long int) ((unsigned char *) &((sec_rgy_unix_group_t *) 0)->name - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((sec_rgy_unix_group_t *) 0)->gid - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((sec_rgy_unix_group_t *) 0)->members - (unsigned char *) 0),
    sizeof(rs_cache_data_t),
    (unsigned long int) ((unsigned char *) &((rs_cache_data_t *) 0)->site_id.time_low - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((rs_cache_data_t *) 0)->site_id.time_mid - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((rs_cache_data_t *) 0)->site_id.time_hi_and_version - (unsigned char *) 0),
    sizeof(sec_rgy_unix_passwd_t),
    (unsigned long int) ((unsigned char *) &((sec_rgy_cursor_t *) 0)->source.clock_seq_hi_and_reserved - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((sec_rgy_cursor_t *) 0)->source.clock_seq_low - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((sec_rgy_cursor_t *) 0)->source.node - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((sec_rgy_cursor_t *) 0)->handle - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((sec_rgy_cursor_t *) 0)->valid - (unsigned char *) 0),
    sizeof(struct {long int name_len; sec_rgy_name_t name;}),
    (unsigned long int) ((unsigned char *) &((struct {long int name_len; sec_rgy_name_t name;} *)0)->name_len
			 - (unsigned char *) 0),
    (unsigned long int) ((unsigned char *) &((struct {long int name_len; sec_rgy_name_t name;} *)0)->name - (unsigned char *) 0),
};
