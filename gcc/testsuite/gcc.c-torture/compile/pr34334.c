typedef unsigned int size_t;
__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef __quad_t __off64_t;
typedef __mode_t mode_t;
typedef __off64_t off_t;
struct timeval   {};
typedef struct   {} fd_set;
typedef union {} __pthread_slist_t;
typedef union {
    struct __pthread_mutex_s   { __extension__ union { };   } __data;
};
extern int stat64 (__const char *__restrict __file,      struct stat64 *__restrict __buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int fstatat64 (int __fd, __const char *__restrict __file,         struct stat64 *__restrict __buf, int __flag)      __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
enum __socket_type { SOCK_STREAM = 1, };
enum { SI_ASYNCNL = -60, };
enum { CLD_EXITED = 1, };
typedef struct sigaltstack   { } stack_t;
enum __rlimit_resource { __RLIMIT_SIGPENDING = 11, };
struct rlimit   { };
enum __priority_which { PRIO_PROCESS = 0, };
typedef union   { } __WAIT_STATUS __attribute__ ((__transparent_union__));
union wait   {
    struct       {       } __wait_stopped;
};
typedef enum {  P_ALL, } idtype_t;
struct utsname   { };
enum   { IPPROTO_IP = 0,   };
enum   { IPPORT_ECHO = 7,   };
struct in_addr   { };
struct in6_addr   {
    union       {       } in6_u;
};
typedef long int wchar_t;
typedef unsigned char guint8;
typedef signed int gint32;
typedef unsigned int guint32;
typedef signed int gssize;
typedef unsigned int gsize;
struct _GStaticMutex {
    union {   } static_mutex;
};
union _GSystemThread { };
typedef int GPid;
typedef char gchar;
typedef int gint;
typedef gint gboolean;
typedef unsigned short gushort;
typedef unsigned long gulong;
typedef unsigned int guint;
typedef void* gpointer;
typedef const void *gconstpointer;
typedef gboolean (*GEqualFunc) (gconstpointer a, gconstpointer b);
typedef void (*GFunc) (gpointer data,   gpointer user_data);
typedef void (*GHFunc) (gpointer key,   gpointer user_data);
struct _GTimeVal { };
typedef struct _GByteArray GByteArray;
guint8* g_byte_array_free (GByteArray *array,      guint index_);
typedef guint32 GQuark;
typedef struct _GError GError;
GError* g_error_new (GQuark domain,   const gchar *message);
gboolean g_error_matches (const GError *error,   gint code);
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
typedef enum { G_USER_DIRECTORY_DESKTOP, } GUserDirectory;
typedef enum { G_THREAD_PRIORITY_URGENT } GThreadPriority;
struct _GThread { };
typedef struct _GCond GCond;
struct _GThreadFunctions {
    void (*cond_wait) (GCond *cond,  GError **error);
    gboolean (*thread_equal) (gpointer thread1,        gpointer thread2);
};
typedef struct _GAsyncQueue GAsyncQueue;
void g_async_queue_sort (GAsyncQueue *queue,           guint *save);
struct tm { };
typedef struct __locale_struct { } *__locale_t;
extern int getaddrinfo (__const char *__restrict __name, struct addrinfo **__restrict __pai);
typedef struct _IO_FILE FILE;
__strsep_1c (char **__s, char __reject) { }
__strsep_2c (char **__s, char __reject1, char __reject2) { }
typedef struct stack_st  { } STACK;
typedef struct asn1_string_st ASN1_BIT_STRING;
typedef struct bn_mont_ctx_st BN_MONT_CTX;
typedef struct evp_cipher_st EVP_CIPHER;
typedef struct EDIPartyName_st {
    union { } d;
} GENERAL_NAME;
typedef struct DIST_POINT_NAME_st {
    union { } name;
} DIST_POINT_NAME;
typedef struct SXNET_st { } NOTICEREF;
typedef struct GENERAL_SUBTREE_st { } X509_PURPOSE;
int X509V3_add_value(const char *name, const char *value, STACK **extlist);
int X509_PURPOSE_add(int id, int trust, int flags, char *name, char *sname, void *arg);
extern char *dcgettext (__const char *__domainname, __const char *__msgid, int __category) __attribute__ ((__nothrow__)) __attribute__ ((__format_arg__ (2)));
enum { __LC_CTYPE = 0, };
struct lconv { };
typedef enum gftp_logging_level_tag { gftp_logging_send, } gftp_logging_level;
struct gftp_file_tag {
    char *file,        *destfile;
    unsigned int selected : 1,
                is_fd : 1;
    gint32 ipv4_network_address, ipv4_netmask;
} gftp_proxy_hosts;
typedef enum { gftp_option_type_text = 0, } gftp_option_type_enum;
typedef struct gftp_config_list_vars_tag { } gftp_config_list_vars;
typedef struct gftp_config_vars_tag { } gftp_config_vars;
typedef struct gftp_option_type_tag {
    int (*read_function) (char *str, gftp_config_vars * cv, int line);
    int (*write_function) (gftp_config_vars * cv, char *buf, size_t buflen, int to_config_file);
} gftp_option_type_var;
typedef struct gftp_request_tag gftp_request;
typedef void (*gftp_logging_func) ( gftp_logging_level level, const char *string, ... );
typedef struct gftp_transfer_tag {
    gftp_request * fromreq, * toreq;
    unsigned int cancel : 1,
                skip_file : 1;
    long numfiles,  resumed_bytes;
} gftp_transfer;
typedef struct gftp_log_tag {
    unsigned int shown : 1,
	use_threads : 1;
} supported_gftp_protocols;
void
gftp_config_parse_args (char *str, int numargs, int lineno, char **first, ...)
{
    char *curpos, *endpos, *pos, **dest, tempchar;
    va_list argp;
    dest = first;
    while (numargs > 0)
    {
        if (numargs > 1)
	{  
	    dest = __builtin_va_arg(argp,char **); 
	    *dest = ((void *)0);
	} 
	numargs--; 
	**dest = '\0'; 
    }
}
