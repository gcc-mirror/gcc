/* PR rtl-optimization/16199 */
/* Origin: Olaf Klein <oklein@smallo.ruhr.de> */

typedef enum {
    APR_LOCK_FCNTL,
    APR_LOCK_FLOCK,
    APR_LOCK_SYSVSEM,
    APR_LOCK_PROC_PTHREAD,
    APR_LOCK_POSIXSEM,
    APR_LOCK_DEFAULT
} apr_lockmech_e;

struct apr_proc_mutex_unix_lock_methods_t {
    unsigned int flags;
    const char *name;
};

typedef struct apr_proc_mutex_unix_lock_methods_t apr_proc_mutex_unix_lock_methods_t;

extern const apr_proc_mutex_unix_lock_methods_t apr_proc_mutex_unix_sysv_methods;

struct apr_proc_mutex_t {
    const apr_proc_mutex_unix_lock_methods_t *inter_meth;
    int curr_locked;
    char *fname;
};

typedef struct apr_proc_mutex_t apr_proc_mutex_t;

extern const apr_proc_mutex_unix_lock_methods_t apr_proc_mutex_unix_proc_pthread_methods;

extern const apr_proc_mutex_unix_lock_methods_t apr_proc_mutex_unix_fcntl_methods;

static int proc_mutex_choose_method(apr_proc_mutex_t *new_mutex, apr_lockmech_e mech)
{
    switch (mech) {
    case APR_LOCK_FCNTL:
        new_mutex->inter_meth = &apr_proc_mutex_unix_fcntl_methods;
        break;
    case APR_LOCK_FLOCK:
        return ((20000 + 50000) + 23);
        break;
    case APR_LOCK_SYSVSEM:
        new_mutex->inter_meth = &apr_proc_mutex_unix_sysv_methods;
        break;
    case APR_LOCK_POSIXSEM:
        return ((20000 + 50000) + 23);
        break;
    case APR_LOCK_PROC_PTHREAD:
        new_mutex->inter_meth = &apr_proc_mutex_unix_proc_pthread_methods;
        break;
    case APR_LOCK_DEFAULT:
        new_mutex->inter_meth = &apr_proc_mutex_unix_proc_pthread_methods;
        break;
    default:
        return ((20000 + 50000) + 23);
    }
    return 0;
}

const char* apr_proc_mutex_defname(void)
{
    apr_proc_mutex_t mutex;

    if (proc_mutex_choose_method(&mutex, APR_LOCK_DEFAULT) != 0) {
        return "unknown";
    }
}
