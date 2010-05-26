/* { dg-options "-Wno-overflow" } */

#ifndef __SIZEOF_INT128__
#define UNACCEPT 0xffffffffffffffffLL
#else
#define UNACCEPT ~((unsigned __int128) 0)
#endif

enum test {
  acceptable = -1,
  unacceptable = UNACCEPT
}; // { dg-error "" }

enum test t = acceptable, u = unacceptable;

int main() {
    return 0;
}
