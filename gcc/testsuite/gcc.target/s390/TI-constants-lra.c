/* { dg-do compile { target int128 } } */
/* { dg-options "-O3" } */

/* 2x lghi */
__int128 a() {
  return 0;
}

/* 2x lghi */
__int128 b() {
  return -1;
}

/* 2x lghi */
__int128 c() {
  return -2;
}

/* lghi + llilh */
__int128 d() {
  return 16000 << 16;
}

/* lghi + llihf */
__int128 e() {
  return (unsigned long long)80000 << 32;
}

/* lghi + llihf */
__int128 f() {
  return (unsigned __int128)80000 << 96;
}

/* llihf + llihf - this is handled via movti_bigconst pattern */
__int128 g() {
  return ((unsigned __int128)80000 << 96) | ((unsigned __int128)80000 << 32);
}

/* Literal pool */
__int128 h() {
  return ((unsigned __int128)80000 << 32) | 1;
}

/* Literal pool */
__int128 i() {
  return (((unsigned __int128)80000 << 32) | 1) << 64;
}
