/* PR c++/25632  */

/* { dg-do compile } */

#define unsigned
__extension__ typedef __SIZE_TYPE__ ssize_t;
#undef unsigned

struct sockaddr_un {
    char sun_path[1];
};
const unsigned SI_SUN_HEAD_LEN = (ssize_t)(((struct sockaddr_un *)0)->sun_path);
int SiGetPeerName ()
{
    return SI_SUN_HEAD_LEN;
}
int SiAccept ()
{
    return SI_SUN_HEAD_LEN;
}

