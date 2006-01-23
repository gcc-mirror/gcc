/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef u_int32_t db_pgno_t;
typedef struct __db DB;
typedef struct __db_env DB_ENV;
typedef struct __db_mpoolfile DB_MPOOLFILE;
typedef struct __dbc DBC;
struct __db {
    DB_MPOOLFILE *mpf;
    db_pgno_t meta_pgno;
    struct __cq_aq {
    } s_links;
};
struct __db_env {
    struct {
    } xa_txn;
    u_int32_t flags;
};
typedef enum { MU_REMOVE, MU_RENAME, MU_OPEN } mu_action;
typedef struct __dbpginfo {
    u_int8_t type;
} PAGE;
int __db_master_update(mdbp, sdbp, txn, subdb, type, action, newname, flags)
  DB *mdbp, *sdbp;
{
    DB_ENV *dbenv;
    DBC *dbc, *ndbc;
    PAGE *p, *r;
    int modify, ret, t_ret;
    if ((ret = __db_cursor(mdbp, txn, &dbc,
			   (((dbenv)->flags & (0x0000002))
			    && modify) ? 35 : 0)) != 0)
	goto err;
    switch (action) {
	case MU_REMOVE:
	    if ((ret = __memp_fget(mdbp->mpf, &sdbp->meta_pgno, 0, &p)) != 0)
		goto err;
	    if ((((PAGE *)p)->type) == 9) {
		if ((ret = __db_free(dbc, r)) != 0) { }
	    }
	    if ((ret = __db_free(dbc, p)) != 0) {
		p = ((void *)0);
		goto err;
	    }
	    p = ((void *)0);
    }
 err:
    if (ndbc != ((void *)0) && (t_ret = __db_c_close(ndbc)) != 0 && ret == 0)
	ret = t_ret;
    return (ret);
}
 
