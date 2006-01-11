/* PR c++/25632  */

/* { dg-do compile } */

struct sockaddr_un {
    char sun_path[1];
};
const unsigned SI_SUN_HEAD_LEN = (long)(((struct sockaddr_un *)0)->sun_path);
int SiGetPeerName ()
{
    return SI_SUN_HEAD_LEN;
}
int SiAccept ()
{
    return SI_SUN_HEAD_LEN;
}

