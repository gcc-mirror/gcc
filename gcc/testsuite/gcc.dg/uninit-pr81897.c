/* { dg-do compile }  */
/* { dg-options "-O2 -Wuninitialized" } */

int f(void);
static inline void rcu_read_unlock(void)
{
        static _Bool __warned;
        if (f() && !__warned && !f()) {
                __warned = 1;
        }
}
int inet6_rtm_getroute(void)
{
        int dst;
        int fibmatch = f();

        if (!fibmatch)
                dst = f();
        rcu_read_unlock();
        if (fibmatch)
                dst = 0;

        return dst;
}
