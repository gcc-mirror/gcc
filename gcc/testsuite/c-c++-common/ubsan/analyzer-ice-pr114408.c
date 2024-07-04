/* { dg-do run } */
/* { dg-require-effective-target analyzer } */
/* { dg-options "-fanalyzer -fsanitize=undefined" } */

int main(){}

int HMAP_unset_copy(const char *key) {
    return __builtin_strcmp("a", key) + __builtin_strcmp("a", key);
}
