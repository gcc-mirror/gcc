/* { dg-do compile } */
/* { dg-options "-O3 -ftree-pre" } */

int f(void);
void acceptloop_th(int *t, int options) {
    if (f()) options |= 0x1 << 0;
    if (f()) options |= 0x1 << 1;
    if (f()) options |= 0x1 << 2;
    if (f()) options |= 0x1 << 3;
    if (f()) options |= 0x1 << 4;
    if (f()) options |= 0x1 << 5;
    if (f()) options |= 0x1 << 6;
    if (f()) options |= 0x1 << 7;
    if (f()) options |= 0x1 << 8;
    if (f()) options |= 0x1 << 9;
    if (f()) options |= 0x1 << 10;
    if (f()) options |= 0x1 << 11;
    if (f()) options |= 0x1 << 12;
    if (f()) options |= 0x1 << 13;
    if (f()) options |= 0x1 << 14;
    if (f()) options |= 0x1 << 15;
    if (f()) options |= 0x1 << 16;
    if (f()) options |= 0x1 << 17;
    if (f()) options |= 0x1 << 18;
    if (f()) options |= 0x1 << 19;
    if (f()) options |= 0x1 << 20;
    if (f()) options |= 0x1 << 21;
    if (f()) options |= 0x1 << 22;
    if (f()) options |= 0x1 << 23;
    if (f()) options |= 0x1 << 24;
    if (f()) options |= 0x1 << 25;
    if (f()) options |= 0x1 << 26;
    if (f()) *t = options;
}

