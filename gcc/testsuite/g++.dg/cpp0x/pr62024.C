// PR c/62024
// { dg-do compile { target c++11 } }
// { dg-require-effective-target sync_char_short }

int *p;
static_assert (__atomic_always_lock_free (1, p), "");
static_assert (__atomic_always_lock_free (1, 0), "");
