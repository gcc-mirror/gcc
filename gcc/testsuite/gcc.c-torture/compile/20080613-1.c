/* PR middle-end/36520 */
/* Testcase by Richard Guenther <rguenth@gcc.gnu.org> */

typedef __SIZE_TYPE__ size_t;
typedef unsigned short int sa_family_t;
struct cmsghdr   {
    size_t cmsg_len;
    __extension__ unsigned char __cmsg_data [];
};
typedef unsigned int uint32_t;
struct in6_addr   {
    union       {
        uint32_t u6_addr32[4];
    } in6_u;
};
struct sockaddr_in   {
    sa_family_t sin_family;
};
struct in6_pktinfo   {
    struct in6_addr ipi6_addr;
};
typedef union {
    struct sockaddr_in sin;
} sockaddr_any;
static sockaddr_any src_addr;

inline struct cmsghdr * cmsg_put(struct cmsghdr *cm, int type, void *data, size_t len)
{
    memcpy(((cm)->__cmsg_data), data, len);
}

int hop_sendmsg(int fd) {
    struct cmsghdr *cm;
    if (src_addr.sin.sin_family) {
        if (src_addr.sin.sin_family == 2) {
            struct in6_pktinfo info;
            cm = cmsg_put(cm, 50, &info, sizeof(info));
        }
    }
}
