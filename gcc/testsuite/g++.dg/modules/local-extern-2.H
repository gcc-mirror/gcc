// { dg-additional-options -fmodule-header }

// { dg-module-cmi {} }

inline int *wcstok(int *__wcstok_ws1)
{
    extern int *__iso_wcstok(int * bob);

    return __iso_wcstok(__wcstok_ws1);
}
