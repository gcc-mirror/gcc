/* PR c++/25632  */

/* { dg-do compile } */

__extension__ typedef __INTPTR_TYPE__ intptr_t;

struct sockaddr_un {
    char sun_path[1];
};
const unsigned SI_SUN_HEAD_LEN = (intptr_t)(((struct sockaddr_un *)0)->sun_path);
int SiGetPeerName ()
{
    return SI_SUN_HEAD_LEN;
}
int SiAccept ()
{
    return SI_SUN_HEAD_LEN;
}

