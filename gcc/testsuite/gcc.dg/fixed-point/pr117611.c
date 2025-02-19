/* { dg-do compile } */
/* { dg-options "-Os" } */

_Accum acc1 (_Accum x)
{
    return x << 16;
}
