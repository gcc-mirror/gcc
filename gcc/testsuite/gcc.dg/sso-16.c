/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O3" } */

typedef __INT32_TYPE__ int32_t;

#define BIG_ENDIAN   __attribute__((scalar_storage_order("big-endian")))

/* host order version (little endian)*/
struct _ip6_addr {
    union {
        char addr8[16];
        int32_t  addr32[4];
    } u;
};

typedef struct _ip6_addr t_ip6_addr;

struct _net_addr {
    char is_v4;
    union {
        int32_t        addr;
        t_ip6_addr addr6;
    } u;
};

typedef struct _net_addr t_net_addr;

/* big endian version */
struct _be_ip6_addr {
    union {
        char addr8[16];
    } BIG_ENDIAN u;
} BIG_ENDIAN;

typedef struct _be_ip6_addr t_be_ip6_addr;

struct _be_net_addr {
    char is_v4;
    union {
        t_be_ip6_addr addr6;
        int32_t           addr;
    } BIG_ENDIAN u;
} BIG_ENDIAN;

typedef struct _be_net_addr t_be_net_addr;

/* convert */
t_be_ip6_addr be_ip6_addr(const t_ip6_addr ip6)
{
    t_be_ip6_addr rc = {
        .u.addr8[0] = ip6.u.addr8[0],
        .u.addr8[1] = ip6.u.addr8[1],
        .u.addr8[2] = ip6.u.addr8[2],
        .u.addr8[3] = ip6.u.addr8[3],
        .u.addr8[4] = ip6.u.addr8[4],
        .u.addr8[5] = ip6.u.addr8[5],
        .u.addr8[6] = ip6.u.addr8[6],
        .u.addr8[7] = ip6.u.addr8[7],
        .u.addr8[8] = ip6.u.addr8[8],
        .u.addr8[9] = ip6.u.addr8[9],
        .u.addr8[10] = ip6.u.addr8[10],
        .u.addr8[11] = ip6.u.addr8[11],
        .u.addr8[12] = ip6.u.addr8[12],
        .u.addr8[13] = ip6.u.addr8[13],
        .u.addr8[14] = ip6.u.addr8[14],
        .u.addr8[15] = ip6.u.addr8[15],
    };
    return rc;
}

t_be_net_addr __attribute__((noipa)) be_net_addr(const t_net_addr ip)
{
    t_be_net_addr rc = {.is_v4 = ip.is_v4 };
    if (ip.is_v4) {
        rc.u.addr = ip.u.addr;
    } else {
        rc.u.addr6 = be_ip6_addr(ip.u.addr6);
    }
    return rc;
}

int main(void)
{
    t_be_net_addr out = { };

    t_net_addr in = {
        .is_v4 = 0,
        .u.addr6.u.addr8 =
            { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }
    };

    out = be_net_addr(in);

    // actually first 4 bytes are swapped
    if (in.u.addr6.u.addr8[0] != out.u.addr6.u.addr8[0])
        __builtin_abort();

    return 0;
}
